package sysdes.formapp.server

import sysdes.formapp.http
import sysdes.formapp.http.Method

case class DispatchContext(request: Request, route: Seq[String], params: Map[String, String]) {
  def split: (String, DispatchContext) = (route.head, DispatchContext(request, route.tail, params))

  def withParam(pair: (String, String)): DispatchContext =
    DispatchContext(request, route, params + pair)
}
object DispatchContext {
  def fromRequest(request: Request): DispatchContext = DispatchContext(
    request,
    new Path(request.path).parts,
    Map()
  )
}

private class Path(val parts: Seq[String]) {
  def this(path: String) = {
    this(path.split("/").filter(!_.isEmpty).toSeq match {
      case Nil   => List("/")
      case parts => parts
    })
  }

  def next: Option[Path] = parts.tail match {
    case Nil  => None
    case tail => Some(new Path(tail))
  }

  def split: (String, Option[Path]) = (parts.head, next)

  def splitLast: (Path, String) = {
    val former :+ last = parts
    (new Path(former), last)
  }
}

trait Dispatchable {
  def dispatch(context: DispatchContext): Option[Response]
}

class Dispatcher extends Dispatchable {
  type RouteHandler = (Request, Map[String, String]) => Response

  private var rules: Map[(Method, String), RouteHandler]         = Map()
  private var subRules: Map[String, Dispatchable]                = Map()
  private var variableRules: Map[Method, (String, RouteHandler)] = Map()
  private var variableSubRule: Option[(String, Dispatchable)]    = None

  def dump: Unit = {
    println(rules)
    println(subRules)
    for ((_, dispatcher: Dispatcher) <- subRules) {
      dispatcher.dump
    }
    println(variableRules)
    println(variableSubRule)
  }

  def dispatch(context: DispatchContext): Option[Response] = {
    context.split match {
      case (path, DispatchContext(request, Nil, params)) =>
        val next = DispatchContext(request, Seq("/"), params)
        dispatchMain(path, context)
          .orElse(dispatchSub(path, next))
          .orElse(dispatchVariableMain(path, context))
          .orElse(dispatchVariableSub(path, next))
      case (path, next) =>
        dispatchSub(path, next)
          .orElse(dispatchVariableSub(path, next))
    }
  }

  private def dispatchMain(path: String, context: DispatchContext): Option[Response] =
    rules.get((context.request.method, path)).map(_(context.request, context.params))

  private def dispatchSub(path: String, next: DispatchContext): Option[Response] =
    subRules.get(path).flatMap(_.dispatch(next))

  private def dispatchVariableMain(path: String, context: DispatchContext): Option[Response] =
    variableRules.get(context.request.method).map(h => h._2(context.request, context.params + (h._1 -> path)))

  private def dispatchVariableSub(path: String, next: DispatchContext): Option[Response] =
    variableSubRule.flatMap(h => h._2.dispatch(next.withParam(h._1 -> path)))

  def bind(path: String, method: Method, target: RouteHandler) =
    bindImpl(new Path(path), method, target)

  private def bindImpl(path: Path, method: Method, target: RouteHandler): Unit = {
    path.split match {
      case (head, None) =>
        putMain(head, method, target)
      case (head, Some(tail)) =>
        subRules.get(head) match {
          case Some(dispatcher: Dispatcher) =>
            dispatcher.bindImpl(tail, method, target)
          case _ =>
            val child = new Dispatcher
            child.bindImpl(tail, method, target)
            putSub(head, child)
        }
    }
  }

  def bindSub(path: String, target: Dispatchable) =
    bindSubImpl(new Path(path), target)

  private def bindSubImpl(path: Path, target: Dispatchable): Unit = {
    path.split match {
      case (head, None) =>
        putSub(head, target)
      case (head, Some(tail)) =>
        subRules.get(head) match {
          case Some(dispatcher: Dispatcher) =>
            dispatcher.bindSubImpl(tail, target)
          case _ =>
            val child = new Dispatcher
            child.bindSubImpl(tail, target)
            putSub(head, child)
        }
    }
  }

  private def putMain(path: String, method: Method, target: RouteHandler): Unit =
    path match {
      case s":$name" => variableRules += method   -> (name, target)
      case normal    => rules += (method, normal) -> target
    }

  private def putSub(path: String, target: Dispatchable): Unit =
    path match {
      case s":$name" => variableSubRule = Some((name, target))
      case normal    => subRules += path -> target
    }
}

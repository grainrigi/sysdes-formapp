package sysdes.formapp.server

import sysdes.formapp.http;
import scala.language.experimental.macros;
import scala.reflect.macros.whitebox.Context;
import scala.annotation.StaticAnnotation;

class ServiceImpl(val c: Context) {
  import c.universe._;

  private val reqT = c.mirror.staticClass("sysdes.formapp.server.Request").toType

  def impl[T: WeakTypeTag] = {
    val tpe = weakTypeOf[T]
    tpe.decls
      .collect {
        case m: MethodSymbol =>
          val adn  = m.name.toString + "__AnotData"
          tpe.decls.find(_.name.toString == adn)
          .map(data => data.as)
      }
    c.Expr(q"println(0)")
  }
}

class MethodAnotImpl(val c: Context) {
  import c.universe._;
  def impl(annottees: c.Expr[Any]*): c.Expr[Any] = {
    annottees.map(_.tree) match {
      case (methodDef: DefDef) :: Nil => {
        val anotData = c.prefix.tree match {
          case q"new ${Ident(TypeName(meth))}(${Literal(Constant(path: String))})" =>
            http
              .Method(meth.toUpperCase())
              .map((_, path))
          case _ => None
        }
        anotData match {
          case Some(anotData) =>
            MethodAnotImpl.anotDatas += ()
            c.Expr(q"""$methodDef ; val ${TermName(methodDef.name.toString + "__AnotData")} = (${Select(
              Select(
                Select(Select(Ident(TermName("sysdes")), TermName("formapp")), TermName("http")),
                TermName("Method")
              ),
              TermName(anotData._1)
            )}, ${anotData._2})""")
          case _ => c.Expr(methodDef)
        }
        c.Expr(methodDef)
      }
      case _ => c.abort(c.enclosingPosition, "Invalid annottees")
    }
  }
}

object MethodAnotImpl {
  var anotDatas: Map[String, (http.Method, String)] = Map()
}

class ServiceAnotImpl(val c: Context) {
  import c.universe._
  def impl(annottees: c.Expr[Any]*): c.Expr[Any] = {
    annottees.map(_.tree) match {
      case (classDef: ClassDef) :: Nil => {
        val (rendezvous, others) = splitRendezvous(classDef)
        others.collect {
          case (methodDef: DefDef) =>
            methodDef.mods.annotations.collect {
              case Apply(Select(New(Ident(TypeName(method))), termNames.CONSTRUCTOR), List(Literal(Constant(path)))) =>
                println(method, path)
            }
        }
        c.Expr(classDef)
      }
      case _ => c.abort(c.enclosingPosition, "Invalid annottees")
    }
  }

  private def splitRendezvous(classDef: ClassDef) = {
    val (rendezvousL, others) = classDef.impl.body.partition(_ match {
      case d: DefDef => d.name == TermName("macroRendezvousPoint")
      case _         => false
    })
    if (rendezvousL.length == 0)
      c.abort(c.enclosingPosition, "@service annotation must be applied to a subtype of Service.")
    (rendezvousL(0), others)
  }

}
class service extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro ServiceAnotImpl.impl
}

class ServiceMethodAnnotationBase extends StaticAnnotation {}

class post(path: String) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro MethodAnotImpl.impl
}

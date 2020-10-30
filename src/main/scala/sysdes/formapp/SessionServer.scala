package sysdes.formapp

import java.net.Socket
import java.util.UUID

import sysdes.formapp.http.Cookies
import sysdes.formapp.server.{Handler, Server}

import scala.collection.mutable

object SessionServer extends Server(8002) {
  override def getHandler(socket: Socket) = new SessionServerHandler(socket)
}

object SessionServerHandler {
  // インスタンス間で共有する内部状態に関する変数・関数はこの中に記述
  val states: mutable.HashMap[UUID, State] = mutable.HashMap()
}

class State(var name: String, var gender: String, var message: String)

class SessionServerHandler(socket: Socket) extends Handler(socket) {
  import sysdes.formapp.server.{NotFound, Ok, Request, Response}
  import SessionServerHandler._
  import http.Util._

  def handle(request: Request): Response = {
    val (id, state) = extractState(request.headers)
    val response    = handleForm(request, state)
    for (uid <- id) response.addHeader("Set-Cookie", s"""session-id=$uid""")
    response
  }

  def handleForm(request: Request, state: State): Response = request match {
    case Request("GET", "/", _, _, _)                  => index()
    case Request("POST", "/", _, _, _)                 => index()
    case Request("POST", "/name", _, _, _)             => name()
    case Request("POST", "/gender", _, _, Some(body))  => gender(body, state)
    case Request("POST", "/message", _, _, Some(body)) => message(body, state)
    case Request("POST", "/confirm", _, _, Some(body)) => confirm(body, state)
    case _                                             => NotFound(s"Requested resource '${request.path}' for ${request.method} is not found.")
  }

  def index(): Response = {
    Ok(genForm("/name", "アンケート開始", "start"))
  }

  def name(): Response = {
    Ok(genForm("/gender", """名前: <input name="name"> """, "next"))
  }

  def gender(body: String, state: State): Response = {
    state.name = parseFormData(body)("name")
    Ok(
      genForm(
        "/message",
        """ 性別: <input type="radio" name="gender" value="male" required> 男性 <input type="radio" name="gender" value="female"> 女性 """,
        "next"
      )
    )
  }

  def message(body: String, state: State): Response = {
    state.gender = parseFormData(body)("gender")
    Ok(
      genForm(
        "/confirm",
        """ メッセージ: <br> <textarea name="message"></textarea> """,
        "next"
      )
    )
  }

  def confirm(body: String, state: State): Response = {
    state.message = parseFormData(body)("message")
    val name    = escapeHTMLSpecialChars(state.name)
    val gender  = escapeHTMLSpecialChars(state.gender)
    val message = escapeHTMLSpecialChars(state.message)
    Ok(
      genForm(
        "/",
        s""" 名前: $name <br> 性別: $gender <br> メッセージ: <br> <textarea disabled>$message</textarea> """,
        "submit"
      )
    )
  }

  def genForm(action: String, input: String, button: String): String = {
    s"""<html>
       |<head><meta charset="utf-8"></head>
       |<body>
       |    <form action="$action" method="post">
       |        $input <br>
       |        <input type="submit" value="$button" />
       |    </form>
       |</body>
       |</html>""".stripMargin
  }

  def extractState(headers: mutable.HashMap[String, String]): (Option[UUID], State) = {
    (for {
      raw   <- headers.get("cookie")
      id    <- new Cookies(raw).values.get("session-id")
      state <- states.get(UUID.fromString(id))
    } yield state) match {
      case Some(state) => (None, state)
      case None =>
        val state = new State("", "", "")
        val id    = UUID.randomUUID()
        states.put(id, state)
        (Some(id), state)
    }
  }
}

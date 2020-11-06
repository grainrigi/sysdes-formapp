package sysdes.formapp

import sysdes.formapp.http.Method._
import java.net.{Socket, URLEncoder}

import sysdes.formapp.server.{DispatchContext, Dispatcher, Handler, Server, Signer}

object StatelessServer extends Server(8001) {
  override def getHandler(socket: Socket) = new StatelessServerHandler(socket)
}

class StatelessServerHandler(socket: Socket) extends Handler(socket) {
  import http.Util
  import http.Util.escapeHTMLSpecialChars
  import sysdes.formapp.server.{NotFound, Ok, Request, Response}

  val dispatcher: Dispatcher = new Dispatcher

  dispatcher.bind("/", GET, (_, _) => index)
  dispatcher.bind("/", POST, (_, _) => index)
  dispatcher.bind("/name", POST, (_, _) => name())
  dispatcher.bind("/gender", POST, (req, _) => gender(req.body.get))
  dispatcher.bind("/message", POST, (req, _) => message(req.body.get))
  dispatcher.bind("/confirm", POST, (req, _) => confirm(req.body.get))

  val signer = new Signer(
    "MFkwEwYHKoZIzj0CAQYIKoZIzj0DAQcDQgAEL/noBBqPoMRziA1hOEpEEwUmKPq5lEQDg4SemkAyxpfcMYCLsqLZYO8fU3g4937nPmZs/pikmBSNT7YZVQD6Dg==",
    "MIGHAgEAMBMGByqGSM49AgEGCCqGSM49AwEHBG0wawIBAQQgvpRq64u/d6jIi5YGr7d2ZyMTOyLlrIF7RMjDZ53GZpahRANCAAQv+egEGo+gxHOIDWE4SkQTBSYo+rmURAODhJ6aQDLGl9wxgIuyotlg7x9TeDj3fuc+Zmz+mKSYFI1PthlVAPoO"
  )

  override def handle(request: Request): Response = {
    val response = dispatcher
      .dispatch(DispatchContext.fromRequest(request))
      .getOrElse(NotFound(s"Requested resource '${request.path}' for ${request.method} is not found."))

    println(response)
    response
  }

  def index(): Response = {
    Ok(genForm("name", Map(), "アンケート開始", "start"))
  }

  def name(): Response = {
    Ok(genForm("gender", Map(), """名前: <input name="name"> """, "next"))
  }

  def gender(body: String): Response = {
    Ok(
      genForm(
        "message",
        parseFormData(body),
        """ 性別: <input type="radio" name="gender" value="male" required> 男性 <input type="radio" name="gender" value="female"> 女性 """,
        "next"
      )
    )
  }

  def message(body: String): Response = {
    Ok(
      genForm(
        "confirm",
        parseFormData(body),
        """ メッセージ: <br> <textarea name="message"></textarea> """,
        "next"
      )
    )
  }

  def confirm(body: String): Response = {
    val data    = parseFormData(body)
    val name    = escapeHTMLSpecialChars(data("name"))
    val gender  = escapeHTMLSpecialChars(data("gender"))
    val message = escapeHTMLSpecialChars(data("message"))
    Ok(
      genForm(
        "/",
        data,
        s""" 名前: $name <br> 性別: $gender <br> メッセージ: <br> <textarea disabled>$message</textarea> """,
        "submit"
      )
    )
  }

  def parseFormData(body: String): Map[String, String] = {
    val (session, data) = Util.parseFormData(body).partition(t => t._1 == "signedsession")
    (session
      .get("signedsession")
      .flatMap(signer.verify(_))
      .map(Util.parseFormData(_))
      .getOrElse(Map())) ++ data
  }

  def genForm(action: String, data: Map[String, String], input: String, button: String): String = {
    val signed = signer.sign(map2urlencoded(data))
    val hs     = s"""<input type="hidden" name="signedsession" value="$signed">"""
    s"""<html>
      |<head><meta charset="utf-8"></head>
      |<body>
      |    <form action="$action" method="post">
      |        $hs
      |        $input <br>
      |        <input type="submit" value="$button" />
      |    </form>
      |</body>
      |</html>""".stripMargin
  }

  def map2urlencoded(data: Map[String, String]): String =
    data
      .map(t => URLEncoder.encode(t._1, "UTF-8") + "=" + URLEncoder.encode(t._2, "UTF-8"))
      .mkString("&")
}

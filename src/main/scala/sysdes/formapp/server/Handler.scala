package sysdes.formapp.server

import java.net.Socket

abstract class Handler(socket: Socket) extends Runnable with Logger {

  import java.io.{BufferedReader, InputStreamReader, OutputStreamWriter}
  import java.nio.charset.{StandardCharsets => Charsets}

  private val encode = Charsets.UTF_8

  final def run(): Unit = {
    val in  = new BufferedReader(new InputStreamReader(socket.getInputStream, encode))
    val out = new OutputStreamWriter(socket.getOutputStream, encode)
    try {
      Request.parse(in) match {
        case Some(Request(_, "/favicon.ico", _, _, _)) | Some(Request(_, "/robot.txt", _, _, _)) => { /* ignore */ }
        case Some(request) => {
          requestLog(request)
          val response = this.handle(request)
          responseLog(response)
          out.write(response.toString)
        }
        case None => {
          out.write(BadRequest().toString)
          System.err.println("[ERROR] Illegal request")
        }
      }
    } catch {
      case e: Throwable => e.printStackTrace()
    } finally {
      out.close()
      socket.close()
    }
  }

  def handle(request: Request): Response
}

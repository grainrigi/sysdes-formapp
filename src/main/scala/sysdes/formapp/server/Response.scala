package sysdes.formapp.server

import scala.collection.mutable.HashMap
import sysdes.formapp.http.Status

sealed abstract class Response(status: Status, body: String) {

  import java.nio.charset.{StandardCharsets => Charsets}
  private val encode = Charsets.UTF_8
  private val cr     = System.lineSeparator()

  private val headers: HashMap[String, String] = HashMap()
  private val ss: StringBuilder                = new StringBuilder(body)

  def addHeader(key: String, value: String) = headers put (key, value)

  def removeHeader(key: String) = headers remove (key)

  def write(msg: String): Unit = ss append (msg)

  def writeln(msg: String) = write(s"${msg}${cr}")

  override def toString: String = {
    val responseBody = ss.toString
    responseBody.getBytes(encode).length match {
      case l if l > 0 => addHeader("Content-Length", l.toString)
    }
    val buf = new StringBuilder(s"HTTP/1.1 ${status}${cr}")
    for ((k, v) <- headers) {
      buf.append(s"${k}: ${v}${cr}")
    }
    buf.append(s"${cr}${responseBody}")
    buf.toString()
  }

}

final case class Ok(body: String = "")         extends Response(Status.OK, body)
final case class NotFound(body: String = "")   extends Response(Status.NotFound, body)
final case class BadRequest(body: String = "") extends Response(Status.BadRequest, body)

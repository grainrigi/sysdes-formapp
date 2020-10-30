package sysdes.formapp.http

import java.net.URLDecoder

object Util {
  def parseFormData(raw: String): Map[String, String] =
    raw
      .split("&")
      .filter(_.length > 0)
      .map(p => p.split("=").map(v => URLDecoder.decode(v, "UTF-8")))
      .map(p => p(0) -> p.lift(1).getOrElse(""))
      .toMap

  def escapeHTMLSpecialChars(s: String): String =
    s.collect {
        case '&'  => "&amp;"
        case '"'  => "&quot;"
        case '\'' => "&#039;"
        case '<'  => "&lt;"
        case '>'  => "&gt;"
        case c    => c.toString
      }
      .mkString("")
}

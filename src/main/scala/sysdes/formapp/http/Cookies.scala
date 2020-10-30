package sysdes.formapp.http

class Cookies(raw: String) {
  val values: Map[String, String] = raw.split("; ").map(_.split("=")).map(a => a(0) -> a(1)).toMap
}

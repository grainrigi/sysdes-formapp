package sysdes.formapp.http

trait Method
object Method {
  case object GET     extends Method
  case object HEAD    extends Method
  case object POST    extends Method
  case object PUT     extends Method
  case object DELETE  extends Method
  case object OPTIONS extends Method
  case object PATCH   extends Method

  def apply(name: String): Option[Method] = name match {
    case "GET"     => Some(GET)
    case "HEAD"    => Some(HEAD)
    case "POST"    => Some(POST)
    case "PUT"     => Some(PUT)
    case "DELETE"  => Some(DELETE)
    case "OPTIONS" => Some(OPTIONS)
    case "PATCH"   => Some(PATCH)
    case _         => None
  }
}

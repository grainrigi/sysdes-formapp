package sysdes.formapp.server
import scala.language.experimental.macros;

class Service extends Dispatcher {
  Service.bindAll[Service]
  @post("/hoge")
  def hogeHandler(req: Request) = {}
}

object Service {
  private def bindAll[T]: Unit = macro ServiceImpl.impl[T]
}

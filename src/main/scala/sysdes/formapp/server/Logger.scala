package sysdes.formapp.server

trait Logger {
  def requestLog(request: Request): Unit = {
    println(">> Request >>>>>>>>>>>>>>>>>>>>")
    println(s"Method: ${request.method}")
    println(s"Path: ${request.path}")
    println(s"Version: ${request.version}")
    println("Headers:")
    for ((key, value) <- request.headers) {
      println(s"  ${key} -> ${value}")
    }
    println("Body:")
    request.body.foreach(println(_))
  }

  def responseLog(response: Response): Unit = {
    println("<< Response <<<<<<<<<<<<<<<<<<<")
    println(response)
  }
}

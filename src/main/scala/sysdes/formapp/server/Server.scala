package sysdes.formapp.server

import java.net.{ServerSocket, Socket}

abstract class Server(port: Int) extends App {
  val serverSocket = new ServerSocket(port)

  println(s"Listening: http://localhost:${port}")

  try {
    while (true) new Thread(getHandler(serverSocket.accept())).run()
  } catch {
    case e: Throwable => e.printStackTrace()
  } finally {
    serverSocket.close()
  }

  def getHandler(socket: Socket): Handler
}

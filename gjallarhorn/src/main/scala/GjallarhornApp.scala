import akka.actor.{ActorRef, ActorSystem}
import http.GjallarhornApiRoute
import keymanager.KeyManagerRef

import scala.concurrent.ExecutionContext

class GjallarhornApp extends Runnable {

  implicit val system = ActorSystem("Gjallarhorn")
  implicit val context = system.dispatcher

  private val keyManagerRef: ActorRef = KeyManagerRef("KeyManager", "keyfiles")

  private val apiRoute = GjallarhornApiRoute(keyManagerRef)

  def run(): Unit = {

  }
}

object GjallarhornApp {

  def main(args: Array[String]): Unit = {
    new GjallarhornApp().run()
  }
}

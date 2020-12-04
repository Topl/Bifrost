package requests

import akka.actor.{Actor, ActorRef}
import akka.util.Timeout
import akka.pattern.{ask, pipe}
import io.circe.Json
import utils.Logging

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

/**
  * Manages requests from Gjallarhorn to Bifrost
  * @param ec - the execution context used for Futures.
  */
class RequestsManager (val bifrostActorRef: ActorRef)( implicit ec: ExecutionContext ) extends Actor with Logging {

  import RequestsManager._

  implicit val timeout: Timeout = 30.seconds


  /**
    * Sends requests to bifrost.
    * @param msg - the type of transaction and the transaction parameters in Json
    * @param sendResponseTo - the actor ref to send response to.
    */
  def sendToBifrost(msg: String, sendResponseTo: ActorRef): Unit = {
    val futureResponse = bifrostActorRef ? msg
    futureResponse.pipeTo(sendResponseTo)
  }

  override def receive: Receive = {

    case BifrostRequest(tx: Json) =>
      val from: ActorRef = sender()
      sendToBifrost(s"request from gjallarhorn: $tx", from)

  }
}

object RequestsManager {
  case class BifrostRequest(tx: Json)
}

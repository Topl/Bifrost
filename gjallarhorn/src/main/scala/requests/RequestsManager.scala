package requests

import akka.actor.{Actor, ActorRef}
import akka.util.Timeout
import akka.pattern.{ask, pipe}
import io.circe.Json
import utils.Logging
import wallet.WalletManager.GjallarhornStarted

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.Success

/**
  * Manages requests from Gjallarhorn to Bifrost
  * @param ec - the execution context used for Futures.
  */
class RequestsManager ( implicit ec: ExecutionContext ) extends Actor with Logging {

  import RequestsManager._

  implicit val timeout: Timeout = 30.seconds

  var bifrostActorRef: Option[ActorRef] = None


  /**
    * Sends requests to bifrost.
    * @param msg - the type of transaction and the transaction parameters in Json
    * @param sendResponseTo - the actor ref to send response to.
    */
  def sendToBifrost(msg: String, sendResponseTo: ActorRef): Unit = {
    context.actorSelection("akka.tcp://bifrost-client@127.0.0.1:9087/user/walletConnectionHandler").resolveOne().onComplete {
      case Success(bifrost: ActorRef) =>
        val futureResponse = bifrost ? msg
        futureResponse.pipeTo(sendResponseTo)
      case _ =>
        log.warn("There is no bifrost actor reference to send to.")
    }
  }

  override def receive: Receive = {

    case GjallarhornStarted(bifrostActor: ActorRef) => bifrostActorRef = Some(bifrostActor)

    case AssetRequest(tx: Json) =>
      val from: ActorRef = sender()
      sendToBifrost(s"asset transaction: $tx", from)

    case WalletRequest(params: Json) =>
      val from: ActorRef = sender()
      sendToBifrost(s"wallet request: $params", from)

  }
}

object RequestsManager {
  case class AssetRequest(tx: Json)
  case class WalletRequest(params: Json)
}

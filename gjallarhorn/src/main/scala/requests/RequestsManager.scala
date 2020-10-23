package requests

import akka.actor.{Actor, ActorRef}
import akka.util.Timeout
import akka.pattern.{ask, pipe}
import io.circe.Json
import utils.Logging
import wallet.WalletManager.GjallarhornStarted

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration._
import scala.util.Success


class RequestsManager ( implicit ec: ExecutionContext ) extends Actor with Logging {

  import RequestsManager._

  implicit val timeout: Timeout = 30.seconds

  var bifrostActorRef: Option[ActorRef] = None


  def msgHandler(str: String): Unit = {

  }

  def sendToBifrost(msg: String, gjalApi: ActorRef): Unit = {
    context.actorSelection("akka.tcp://bifrost-client@127.0.0.1:9087/user/walletConnectionHandler").resolveOne().onComplete {
      case Success(bifrost: ActorRef) =>
        val futureResponse = bifrost ? msg
        futureResponse.pipeTo(gjalApi)
      case _ =>
        log.warn("There is no bifrost actor reference to send to.")
    }
  }

  override def receive: Receive = {
    case msg: String => msgHandler(msg)

    case GjallarhornStarted(bifrostActor: ActorRef) => {
      bifrostActorRef = Some(bifrostActor)
    }

    case AssetRequest(tx: Json) => {
      val gjalApi: ActorRef = sender()
      sendToBifrost(s"asset transaction: $tx", gjalApi)
    }

    case WalletRequest(params: Json) => {
      val gjalApi: ActorRef = sender()
      sendToBifrost(s"wallet request: $params", gjalApi)
    }


  }
}

object RequestsManager {
  case class AssetRequest(tx: Json)
  case class WalletRequest(params: Json)
}

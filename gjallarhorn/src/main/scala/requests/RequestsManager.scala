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

  override def receive: Receive = {
    case msg: String => msgHandler(msg)

    case GjallarhornStarted(bifrostActor: ActorRef) => {
      bifrostActorRef = Some(bifrostActor)
    }

    case CreateTransaction(tx: Json) => {
      val gjalApi: ActorRef = sender()
      println("in create transaction in requests manager")
      context.actorSelection("akka.tcp://bifrost-client@127.0.0.1:9087/user/walletConnectionHandler").resolveOne().onComplete {
        case Success(bifrost: ActorRef) =>
          val futureResponse = bifrost ? s"asset transaction: $tx"
          futureResponse.pipeTo(gjalApi)
        case _ =>
          log.warn("There is not bifrost actor reference to send to.")
      }
    }


  }
}

object RequestsManager {
  case class CreateTransaction (tx: Json)
}

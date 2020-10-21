package co.topl.wallet

import akka.actor.{Actor, ActorRef}
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import co.topl.modifier.block.Block
import co.topl.network.NodeViewSynchronizer.ReceivableMessages.{ModificationOutcome, SemanticallySuccessfulModifier}
import co.topl.utils.Logging
import co.topl.wallet.AssetRequests.AssetRequest
import io.circe.Json
import io.circe.parser.parse

import scala.concurrent.{Await, ExecutionContext}
import scala.util.Success
import scala.concurrent.duration._


class WalletConnectionHandler ( implicit ec: ExecutionContext ) extends Actor with Logging {
  import WalletConnectionHandler._

  var remoteWalletActor: Option[ActorRef] = None

  implicit val timeout: Timeout = 10.seconds

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[ModificationOutcome])
  }

  /*
   case NewBlockAdded(block: Block) => {
      remoteWalletActor match {
        case Some(actor) => {
          actor ! s"new block added: ${block.json}"
        }
        case None => System.out.println("no wallet running")
      }
      context.system.eventStream.publish(s"publishing new block: $block")
     /* val lookupBus = new LookupBusImpl
      lookupBus.publish(MsgEnvelope("New block added", block))*/
    }
   */

  def msgHandler(msg: String): Unit = {
    if (msg == "Remote wallet actor initialized") {
      remoteWalletActor = Some(sender())
      remoteWalletActor match {
        case Some(actor) => {
          actor ! s"received new wallet from: ${sender()}"
        }
        case None => println ("no wallets!")
      }
    }
    if (msg == "Remote wallet actor stopped") {
      remoteWalletActor = None
      sender ! s"The remote wallet ${sender()} has been removed from the WalletConnectionHandler in Bifrost"
    }

    if (msg.contains("asset transaction:")) {
      val txString: String = msg.substring("asset transaction: ".length)
      println("Wallet Connection handler received asset transaction: " + txString)
      val walletActorRef: ActorRef = sender()
      parse(txString) match {
        case Right(tx) => {
          context.actorSelection("../" + AssetRequests.actorName).resolveOne().onComplete {
            case Success(request: ActorRef) =>
              val futureResponse = request ? AssetRequest(tx)
              futureResponse.pipeTo(walletActorRef)
            case _ =>
              log.warn("No ledger actor found. Can not update view.")
          }
        }
        case Left(error)  => throw new Exception (s"error: $error")
      }

    }
  }

  override def receive: Receive = {

    case msg: String => {
      msgHandler(msg)
    }

    case GetRemoteWalletRef => sender ! remoteWalletActor

    case SemanticallySuccessfulModifier(block: Block) => {
      remoteWalletActor match {
        case Some(actor) => {
          actor ! s"new block added: ${block.json}"
        }
        case None => System.out.println("no wallet running")
      }
    }


  }
}

object WalletConnectionHandler {
  case object GetRemoteWalletRef
  //case class NewBlockAdded(block: PersistentNodeViewModifier)
}

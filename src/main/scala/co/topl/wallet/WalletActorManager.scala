package co.topl.wallet

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import co.topl.modifier.block.Block
import co.topl.modifier.block.PersistentNodeViewModifier


class WalletActorManager extends Actor {
  import WalletActorManager._

  var remoteWalletActor: Option[ActorRef] = None

  override def receive: Receive = {

    case msg: String => {
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
        sender ! s"The remote wallet ${sender()} has been removed from the WalletActorManager in Bifrost"
      }
    }

    case GetRemoteWalletRef => sender ! remoteWalletActor

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

  }
}

object WalletActorManager {
  case object GetRemoteWalletRef
  case class NewBlockAdded(block: PersistentNodeViewModifier)

  def apply( implicit system: ActorSystem ): ActorRef =
    system.actorOf(Props(new WalletActorManager), name = "walletActorManager")
}

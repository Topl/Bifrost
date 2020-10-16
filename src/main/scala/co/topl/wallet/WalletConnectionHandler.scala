package co.topl.wallet

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import co.topl.modifier.block.Block
import co.topl.network.NodeViewSynchronizer.ReceivableMessages.{ModificationOutcome, SemanticallySuccessfulModifier}


class WalletConnectionHandler extends Actor {
  import WalletConnectionHandler._

  var remoteWalletActor: Option[ActorRef] = None

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
        sender ! s"The remote wallet ${sender()} has been removed from the WalletConnectionHandler in Bifrost"
      }
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

  def apply( implicit system: ActorSystem ): ActorRef =
    system.actorOf(Props(new WalletConnectionHandler), name = "walletConnectionHandler")
}

package bifrost.wallet

import akka.actor.{Actor, ActorRef}

class WalletActorManager extends Actor {
  import WalletActorManager._

  var remoteWalletActor: Option[ActorRef] = None

  override def receive: Receive = {

    case msg: String => {
      if (msg == "Remote wallet actor initialized") {
        remoteWalletActor = Some(sender())
        sender ! s"received new wallet from: ${sender()}"
      }
      if (msg == "Remote wallet actor stopped") {
        remoteWalletActor = None
        sender ! s"the remote wallet ${sender()} has been removed from the WalletActorManager in Bifrost"
      }
    }

    case GetRemoteWalletRef => sender ! remoteWalletActor

  }
}

object WalletActorManager {

  case object GetRemoteWalletRef

}

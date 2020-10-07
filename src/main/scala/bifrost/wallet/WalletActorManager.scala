package bifrost.wallet

import akka.actor.{Actor, ActorRef}

class WalletActorManager extends Actor {
  import WalletActorManager._

  var remoteWalletActor: Option[ActorRef] = None

  override def receive: Receive = {

    case msg: String => {
      if (msg == "Remote wallet actor initialized") {
        remoteWalletActor = Some(sender())
        sender ! s"received new wallet from: ${Some(sender())}"
      }
    }

    case GetRemoteWalletRef => sender ! remoteWalletActor

  }
}

object WalletActorManager {
  case class RemoteWalletActorInitialized()

  case class GetRemoteWalletRef()
}

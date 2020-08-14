package bifrost.network.peer

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import bifrost.network.message.{GetPeersSpec, Message, MessageSpec, PeersSpec}
import bifrost.network.{SendToPeer, SendToPeers, SendToRandom}
import bifrost.settings.{BifrostContext, NetworkSettings}
import bifrost.utils.Logging
import shapeless.syntax.typeable._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.language.postfixOps

/**
  * Responsible for discovering and sharing new peers.
  */
class PeerSynchronizer(networkControllerRef: ActorRef,
                       peerManager: ActorRef,
                       settings: NetworkSettings,
                       bifrostContext: BifrostContext)
                      (implicit ec: ExecutionContext) extends Actor with Logging {

  // Import the types of messages this actor can RECEIVE
  import bifrost.network.SharedNetworkMessages.ReceivableMessages.DataFromPeer

  // Import the types of messages this actor can SEND
  import bifrost.network.NetworkController.ReceivableMessages.{RegisterMessageSpecs, SendToNetwork}
  import bifrost.network.peer.PeerManager.ReceivableMessages.{AddPeerIfEmpty, RecentlySeenPeers}

  private implicit val timeout: Timeout = Timeout(settings.syncTimeout.getOrElse(5 seconds))
  protected val peersSpec: PeersSpec = bifrostContext.peerSyncRemoteMessages.peersSpec
  protected val getPeersSpec: GetPeersSpec = bifrostContext.peerSyncRemoteMessages.getPeersSpec

  override def preStart: Unit = {
    networkControllerRef ! RegisterMessageSpecs(bifrostContext.peerSyncRemoteMessages.toSeq, self)

    val msg = Message[Unit](getPeersSpec, Right(Unit), None)
    context.system.scheduler.schedule(2.seconds, settings.getPeersInterval)(networkControllerRef ! SendToNetwork(msg, SendToRandom))
  }

////////////////////////////////////////////////////////////////////////////////////
////////////////////////////// ACTOR MESSAGE HANDLING //////////////////////////////

  // ----------- CONTEXT && MESSAGE PROCESSING FUNCTIONS
  override def receive: Receive = {
    case DataFromPeer(spec, peers: Seq[PeerSpec]@unchecked, _) if spec.messageCode == PeersSpec.MessageCode && peers.cast[Seq[PeerSpec]].isDefined =>
      peers.foreach(peerSpec => peerManager ! AddPeerIfEmpty(peerSpec))

    case DataFromPeer(spec, _, peer) if spec.messageCode == GetPeersSpec.MessageCode =>
      (peerManager ? RecentlySeenPeers(settings.maxPeerSpecObjects))
        .mapTo[Seq[PeerInfo]]
        .foreach { peers =>
          val msg = Message(peersSpec, Right(peers.map(_.peerSpec)), None)
          networkControllerRef ! SendToNetwork(msg, SendToPeer(peer))
        }

    case nonsense: Any => log.warn(s"PeerSynchronizer: got unexpected input $nonsense from ${sender()}")
  }
}



////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////// COMPANION SINGLETON ////////////////////////////////

object PeerSynchronizer {

  case class RemoteMessageHandler (peersSpec: PeersSpec, getPeersSpec: GetPeersSpec) {
    def toSeq: Seq[MessageSpec[_]] = Seq(peersSpec, getPeersSpec)
  }

}

////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////// ACTOR REF HELPER //////////////////////////////////

object PeerSynchronizerRef {
  def props(networkControllerRef: ActorRef, peerManager: ActorRef, settings: NetworkSettings, bifrostContext: BifrostContext)
           (implicit ec: ExecutionContext): Props =
    Props(new PeerSynchronizer(networkControllerRef, peerManager, settings,  bifrostContext))

  def apply(networkControllerRef: ActorRef, peerManager: ActorRef, settings: NetworkSettings, bifrostContext: BifrostContext)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(props(networkControllerRef, peerManager, settings,  bifrostContext))

  def apply(name: String, networkControllerRef: ActorRef, peerManager: ActorRef, settings: NetworkSettings, bifrostContext: BifrostContext)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(props(networkControllerRef, peerManager, settings,  bifrostContext), name)
}
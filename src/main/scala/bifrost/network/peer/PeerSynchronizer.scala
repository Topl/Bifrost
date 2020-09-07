package bifrost.network.peer

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import bifrost.network.message.{GetPeersSpec, Message, MessageSpec, PeersSpec}
import bifrost.network.{SendToPeer, SendToRandom, Synchronizer}
import bifrost.settings.{BifrostContext, NetworkSettings}
import bifrost.utils.Logging
import shapeless.syntax.typeable._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.language.postfixOps

/**
  * Responsible for discovering and sharing new peers.
  */
class PeerSynchronizer(
    val networkControllerRef: ActorRef,
    peerManager: ActorRef,
    settings: NetworkSettings,
    bifrostContext: BifrostContext
)(implicit ec: ExecutionContext)
    extends Actor
    with Synchronizer
    with Logging {

  // Import the types of messages this actor can SEND
  import bifrost.network.NetworkController.ReceivableMessages.{RegisterMessageSpecs, SendToNetwork}
  import bifrost.network.peer.PeerManager.ReceivableMessages.{AddPeerIfEmpty, RecentlySeenPeers}

  private implicit val timeout: Timeout = Timeout(settings.syncTimeout.getOrElse(5 seconds))

  // types of remote messages to be handled by this synchronizer
  protected val peersSpec: PeersSpec = bifrostContext.peerSyncRemoteMessages.peersSpec
  protected val getPeersSpec: GetPeersSpec = bifrostContext.peerSyncRemoteMessages.getPeersSpec

  // partial functions for identifying local method handlers for the messages above
  protected val msgHandlers: PartialFunction[Message[_], Unit] = {
    case Message(spec, peers: Seq[PeerSpec] @unchecked, _) if spec.messageCode == PeersSpec.MessageCode    => addNewPeers(peers)
    case Message(spec, _, Some(remote))                    if spec.messageCode == GetPeersSpec.MessageCode => gossipPeers(remote)
  }


  override def preStart: Unit = {
    networkControllerRef ! RegisterMessageSpecs(
      bifrostContext.peerSyncRemoteMessages.toSeq,
      self
    )

    val msg = Message[Unit](getPeersSpec, Right(Unit), None)
    context.system.scheduler.schedule(2.seconds, settings.getPeersInterval)(
      networkControllerRef ! SendToNetwork(msg, SendToRandom)
    )
  }

////////////////////////////////////////////////////////////////////////////////////
////////////////////////////// ACTOR MESSAGE HANDLING //////////////////////////////

  // ----------- CONTEXT && MESSAGE PROCESSING FUNCTIONS
  override def receive: Receive = {

    // data received from a remote peer
    case Message(spec, Left(msgBytes), source) =>
      parseAndHandle(spec, msgBytes, source)

    // fall-through method for reporting unhandled messages
    case nonsense: Any => log.warn(s"PeerSynchronizer: got unexpected input $nonsense from ${sender()}")
  }

  /**
    * Handles adding new peers to the peer database if they were previously unknown
    * @param peers sequence of peer specs describing a remote peers details
    */
  private def addNewPeers(peers: Seq[PeerSpec]): Unit =
    if (peers.cast[Seq[PeerSpec]].isDefined) {
      peers.foreach(peerSpec => peerManager ! AddPeerIfEmpty(peerSpec))
    }

  /**
    * Handles gossiping about the locally known peer set to a given remote peer
    * @param remote the remote peer to be informed of our local peers
    */
  private def gossipPeers(remote: ConnectedPeer): Unit =
    (peerManager ? RecentlySeenPeers(settings.maxPeerSpecObjects))
      .mapTo[Seq[PeerInfo]]
      .foreach { peers =>
        val msg = Message(peersSpec, Right(peers.map(_.peerSpec)), None)
        networkControllerRef ! SendToNetwork(msg, SendToPeer(remote))
      }
}

////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////// COMPANION SINGLETON ////////////////////////////////

object PeerSynchronizer {

  case class RemoteMessageHandler(
      peersSpec: PeersSpec,
      getPeersSpec: GetPeersSpec
  ) {
    def toSeq: Seq[MessageSpec[_]] = Seq(peersSpec, getPeersSpec)
  }

}

////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////// ACTOR REF HELPER //////////////////////////////////

object PeerSynchronizerRef {
  def apply(
      networkControllerRef: ActorRef,
      peerManager: ActorRef,
      settings: NetworkSettings,
      bifrostContext: BifrostContext
  )(implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(
      props(networkControllerRef, peerManager, settings, bifrostContext)
    )

  def apply(
      name: String,
      networkControllerRef: ActorRef,
      peerManager: ActorRef,
      settings: NetworkSettings,
      bifrostContext: BifrostContext
  )(implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(
      props(networkControllerRef, peerManager, settings, bifrostContext),
      name
    )

  def props(
      networkControllerRef: ActorRef,
      peerManager: ActorRef,
      settings: NetworkSettings,
      bifrostContext: BifrostContext
  )(implicit ec: ExecutionContext): Props =
    Props(
      new PeerSynchronizer(
        networkControllerRef,
        peerManager,
        settings,
        bifrostContext
      )
    )
}

package bifrost.network.peer

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import bifrost.network.message.{GetPeersSpec, Message, MessageSpec, PeersSpec}
import bifrost.network.{SendToPeer, SendToRandom, message}
import bifrost.settings.{BifrostContext, NetworkSettings}
import bifrost.utils.Logging
import shapeless.syntax.typeable._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.{Failure, Success}

/**
  * Responsible for discovering and sharing new peers.
  */
class PeerSynchronizer(
    networkControllerRef: ActorRef,
    peerManager: ActorRef,
    settings: NetworkSettings,
    bifrostContext: BifrostContext
)(implicit ec: ExecutionContext)
    extends Actor
    with Logging {

  // Import the types of messages this actor can SEND
  import bifrost.network.NetworkController.ReceivableMessages.{PenalizePeer, RegisterMessageSpecs, SendToNetwork}
  import bifrost.network.peer.PeerManager.ReceivableMessages.{AddPeerIfEmpty, RecentlySeenPeers}

  private implicit val timeout: Timeout = Timeout(settings.syncTimeout.getOrElse(5 seconds))

  protected val peersSpec: PeersSpec = bifrostContext.peerSyncRemoteMessages.peersSpec
  protected val getPeersSpec: GetPeersSpec = bifrostContext.peerSyncRemoteMessages.getPeersSpec

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
    case message.Message(spec, Left(msgBytes), Some(remote)) =>
      // attempt to parse the message
      spec.parseBytes(msgBytes) match {
        // if a message could be parsed, match the type of content found and ensure the messageCode also matches
        case Success(content) =>
          content match {
            case peers: Seq[PeerSpec] if spec.messageCode == PeersSpec.MessageCode    => addNewPeers(peers)
            case _                    if spec.messageCode == GetPeersSpec.MessageCode => gossipPeers(remote)
          }

        // if a message could not be parsed, penalize the remote peer
        case Failure(e) =>
          log.error(s"Failed to deserialize data from $remote: ", e)
          networkControllerRef ! PenalizePeer(
            remote.connectionId.remoteAddress,
            PenaltyType.PermanentPenalty
          )
      }

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

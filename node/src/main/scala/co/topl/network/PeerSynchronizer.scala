package co.topl.network

import akka.actor.{ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout
import co.topl.network.NetworkController.ReceivableMessages.{PenalizePeer, RegisterMessages, SendToNetwork}
import co.topl.network.PeerManager.ReceivableMessages.{AddPeerIfEmpty, RecentlySeenPeers}
import co.topl.network.message.Messages.MessagesV1
import co.topl.network.message.{Message, MessageCode, Transmission}
import co.topl.network.peer.{ConnectedPeer, PeerInfo, PeerMetadata, PenaltyType}
import co.topl.settings.AppSettings
import co.topl.utils.{Logging, TimeProvider}
import shapeless.syntax.typeable._

import scala.concurrent.duration._
import scala.language.postfixOps

/** Responsible for discovering and sharing new peers. */
class PeerSynchronizer(
  networkControllerRef: ActorRef,
  peerManager:          ActorRef,
  settings:             AppSettings
) extends Synchronizer
    with Logging {

  import context.dispatcher

  implicit private val timeout: Timeout = Timeout(settings.network.syncTimeout.getOrElse(5 seconds))

  /** partial functions for identifying local method handlers for the messages above */
  protected val msgHandlers: PartialFunction[(Message, ConnectedPeer), Unit] = {
    case (message: MessagesV1.PeersMetadataResponse, _) => addNewPeers(message.peers)
    case (_: MessagesV1.PeersMetadataRequest, remote)   => gossipPeers(remote)
  }

  override def preStart(): Unit = {

    /** register as a handler for synchronization-specific types of messages */
    networkControllerRef ! RegisterMessages(
      PeerSynchronizer.acceptableMessages,
      self
    )

    /** register for application initialization message */
    log.info(s"${Console.YELLOW}PeerSynchronizer transitioning to the operational state${Console.RESET}")
    scheduleGetPeers()
  }

  // //////////////////////////////////////////////////////////////////////////////////
  // //////////////////////////// ACTOR MESSAGE HANDLING //////////////////////////////

  // ----------- CONTEXT ----------- //
  override def receive: Receive =
    processDataFromPeer orElse
    nonsense

  // //////////////////////////////////////////////////////////////////////////////////
  // ////////////////////////////// METHOD DEFINITIONS ////////////////////////////////
  /** Schedule a message to gossip about our locally known peers */
  private def scheduleGetPeers(): Unit = {
    val msg = MessagesV1.PeersMetadataRequest()

    context.system.scheduler.scheduleWithFixedDelay(
      2.seconds,
      settings.network.getPeersInterval,
      networkControllerRef,
      SendToNetwork(Transmission.encodeMessage(msg), msg.version, SendToRandom)
    )
  }

  /**
   * Handles adding new peers to the peer database if they were previously unknown
   *
   * @param peers sequence of peer specs describing a remote peers details
   */
  private def addNewPeers(peers: Seq[PeerMetadata]): Unit =
    if (peers.cast[Seq[PeerMetadata]].isDefined) {
      peers.foreach(peerSpec => peerManager ! AddPeerIfEmpty(peerSpec))
    }

  /**
   * Handles gossiping about the locally known peer set to a given remote peer
   *
   * @param remote the remote peer to be informed of our local peers
   */
  private def gossipPeers(remote: ConnectedPeer): Unit =
    (peerManager ? RecentlySeenPeers(settings.network.maxPeerSpecObjects))
      .mapTo[Seq[PeerInfo]]
      .foreach { peers =>
        val msg = MessagesV1.PeersMetadataResponse(peers.map(_.metadata))
        networkControllerRef ! SendToNetwork(Transmission.encodeMessage(msg), msg.version, SendToPeer(remote))
      }

  override protected def penalizeMaliciousPeer(peer: ConnectedPeer): Unit =
    networkControllerRef ! PenalizePeer(peer.connectionId.remoteAddress, PenaltyType.PermanentPenalty)

  protected def nonsense: Receive = { case nonsense: Any =>
    log.warn(s"NodeViewSynchronizer: got unexpected input $nonsense from ${sender()}")
  }
}

////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////// COMPANION SINGLETON ////////////////////////////////

object PeerSynchronizer {

  val acceptableMessages: Seq[MessageCode] =
    Seq(
      MessagesV1.PeersMetadataResponse.messageCode,
      MessagesV1.PeersMetadataRequest.messageCode
    )

  val actorName = "peerSynchronizer"

}

////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////// ACTOR REF HELPER //////////////////////////////////

object PeerSynchronizerRef {

  def props(
    networkControllerRef:  ActorRef,
    peerManager:           ActorRef,
    settings:              AppSettings
  )(implicit timeProvider: TimeProvider): Props =
    Props(new PeerSynchronizer(networkControllerRef, peerManager, settings))

}

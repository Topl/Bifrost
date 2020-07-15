package bifrost.network.peer

import java.net.{InetAddress, InetSocketAddress}

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import bifrost.settings.BifrostContext
import bifrost.network._
import bifrost.settings.AppSettings
import bifrost.utils.NetworkUtils
import bifrost.utils.Logging

import scala.util.Random

/**
  * Peer manager takes care of peers connected and in process, and also chooses a random peer to connect
  * Must be singleton
  */
class PeerManager(settings: AppSettings, bifrostContext: BifrostContext) extends Actor with Logging {

  import PeerManager.ReceivableMessages._

  private val peerDatabase = new InMemoryPeerDatabase(settings.network, bifrostContext.timeProvider)

  if (peerDatabase.isEmpty) {
    // fill database with peers from config file if empty
    settings.network.knownPeers.foreach { address =>
      if (!isSelf(address)) {
        peerDatabase.addOrUpdateKnownPeer(PeerInfo.fromAddress(address))
      }
    }
  }

////////////////////////////////////////////////////////////////////////////////////
////////////////////////////// ACTOR MESSAGE HANDLING //////////////////////////////

  // ----------- CONTEXT
  override def receive: Receive =
    peersManagement orElse
      apiInterface orElse
      nonsense

  // ----------- MESSAGE PROCESSING FUNCTIONS
  private def peersManagement: Receive = {

    case ConfirmConnection(connectionId, handlerRef) =>
      log.info(s"Connection confirmation request: $connectionId")
      if (peerDatabase.isBlacklisted(connectionId.remoteAddress)) sender() ! ConnectionDenied(connectionId, handlerRef)
      else sender() ! ConnectionConfirmed(connectionId, handlerRef)

    case AddOrUpdatePeer(peerInfo) =>
      // We have connected to a peer and got his peerInfo from him
      if (!isSelf(peerInfo.peerSpec)) peerDatabase.addOrUpdateKnownPeer(peerInfo)

    case Penalize(peer, penaltyType) =>
      log.info(s"$peer penalized, penalty: $penaltyType")
      if (peerDatabase.penalize(peer, penaltyType)) {
        log.info(s"$peer blacklisted")
        peerDatabase.addToBlacklist(peer, penaltyType)
        sender() ! Blacklisted(peer)
      }

    case AddPeerIfEmpty(peerSpec) =>
      // We have received peer data from other peers. It might be modified and should not affect existing data if any
      if (peerSpec.address.forall(a => peerDatabase.get(a).isEmpty) && !isSelf(peerSpec)) {
        val peerInfo: PeerInfo = PeerInfo(peerSpec, 0, None)
        peerDatabase.addOrUpdateKnownPeer(peerInfo)
      }

    case RemovePeer(address) =>
      log.info(s"$address removed")
      peerDatabase.remove(address)

    case get: GetPeers[_] =>
      sender() ! get.choose(peerDatabase.knownPeers, peerDatabase.blacklistedPeers, bifrostContext)
  }

  // TODO: JAA - 2020.07.15 - I don't think this case will ever be hit because the get above will catch the
  // GetAllPeers and GetBlackListedPeers since they both extend type GetPeers
  private def apiInterface: Receive = {

    case GetAllPeers =>
      log.trace(s"Get all peers: ${peerDatabase.knownPeers}")
      sender() ! peerDatabase.knownPeers

    case GetBlacklistedPeers =>
      sender() ! peerDatabase.blacklistedPeers
  }

  private def nonsense: Receive = {
    case nonsense: Any =>
      log.warn(s"PeerManager: got unexpected input $nonsense from ${sender()}")
  }

////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////// METHOD DEFINITIONS ////////////////////////////////

  /**
    * Given a peer's address, returns `true` if the peer is the same is this node.
    */
  private def isSelf(peerAddress: InetSocketAddress): Boolean = {
    NetworkUtils.isSelf(peerAddress, settings.network.bindAddress, bifrostContext.externalNodeAddress)
  }

  private def isSelf(peerSpec: PeerSpec): Boolean = {
    peerSpec.declaredAddress.exists(isSelf) || peerSpec.localAddressOpt.exists(isSelf)
  }

}

////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////// COMPANION SINGLETON ////////////////////////////////

object PeerManager {

  object ReceivableMessages {

    case class ConfirmConnection(connectionId: ConnectionId, handlerRef: ActorRef)

    case class ConnectionConfirmed(connectionId: ConnectionId, handlerRef: ActorRef)

    case class ConnectionDenied(connectionId: ConnectionId, handlerRef: ActorRef)

    case class Penalize(remote: InetSocketAddress, penaltyType: PenaltyType)

    case class Blacklisted(remote: InetSocketAddress)

    // peerListOperations messages
    case class AddOrUpdatePeer(data: PeerInfo)

    case class AddPeerIfEmpty(data: PeerSpec)

    case class RemovePeer(address: InetSocketAddress)

    /**
      * Message to get peers from known peers map filtered by `choose` function
      */
    trait GetPeers[T] {
      def choose(knownPeers: Map[InetSocketAddress, PeerInfo],
                 blacklistedPeers: Seq[InetAddress],
                 bifrostContext: BifrostContext): T
    }

    /**
      * Choose at most `howMany` random peers, which are connected to our peer or
      * were connected in at most 1 hour ago and weren't blacklisted.
      */
    case class RecentlySeenPeers(howMany: Int) extends GetPeers[Seq[PeerInfo]] {
      private val TimeDiff: Long = 60 * 60 * 1000

      override def choose(knownPeers: Map[InetSocketAddress, PeerInfo],
                          blacklistedPeers: Seq[InetAddress],
                          sc: BifrostContext): Seq[PeerInfo] = {
        val currentTime = sc.timeProvider.time()
        val recentlySeenNonBlacklisted = knownPeers.values.toSeq
          .filter { p =>
            (p.connectionType.isDefined || currentTime - p.lastSeen > TimeDiff) &&
              !blacklistedPeers.exists(ip => p.peerSpec.declaredAddress.exists(_.getAddress == ip))
          }
        Random.shuffle(recentlySeenNonBlacklisted).take(howMany)
      }
    }

    case object GetAllPeers extends GetPeers[Map[InetSocketAddress, PeerInfo]] {

      override def choose(knownPeers: Map[InetSocketAddress, PeerInfo],
                          blacklistedPeers: Seq[InetAddress],
                          sc: BifrostContext): Map[InetSocketAddress, PeerInfo] = knownPeers
    }

    case class RandomPeerExcluding(excludedPeers: Seq[PeerInfo]) extends GetPeers[Option[PeerInfo]] {

      override def choose(knownPeers: Map[InetSocketAddress, PeerInfo],
                          blacklistedPeers: Seq[InetAddress],
                          sc: BifrostContext): Option[PeerInfo] = {
        val candidates = knownPeers.values.filterNot { p =>
          excludedPeers.exists(_.peerSpec.address == p.peerSpec.address) &&
            blacklistedPeers.exists(addr => p.peerSpec.address.map(_.getAddress).contains(addr))
        }.toSeq
        if (candidates.nonEmpty) Some(candidates(Random.nextInt(candidates.size)))
        else None
      }
    }

    case object GetBlacklistedPeers extends GetPeers[Seq[InetAddress]] {

      override def choose(knownPeers: Map[InetSocketAddress, PeerInfo],
                          blacklistedPeers: Seq[InetAddress],
                          bifrostContext: BifrostContext): Seq[InetAddress] = blacklistedPeers
    }

  }

}

////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////// ACTOR REF HELPER //////////////////////////////////

object PeerManagerRef {

  def props(settings: AppSettings, bifrostContext: BifrostContext): Props = {
    Props(new PeerManager(settings, bifrostContext))
  }

  def apply(settings: AppSettings, bifrostContext: BifrostContext)
           (implicit system: ActorSystem): ActorRef = {
    system.actorOf(props(settings, bifrostContext))
  }

  def apply(name: String, settings: AppSettings, bifrostContext: BifrostContext)
           (implicit system: ActorSystem): ActorRef = {
    system.actorOf(props(settings, bifrostContext), name)
  }

}

package co.topl.network.peer

import co.topl.settings.NetworkSettings
import co.topl.utils.{Logging, TimeProvider}

import java.net.{InetAddress, InetSocketAddress}
import scala.concurrent.duration._

/**
 * In-memory peer database implementation supporting temporal blacklisting.
 *
 * @param settings network settings
 * @param timeProvider NetworkTimeProvider that provides the current timestamp in milliseconds with ntp offset checked
 */
final class InMemoryPeerDatabase(settings: NetworkSettings, timeProvider: TimeProvider)
    extends PeerDatabase
    with Logging {

  private var peers = Map.empty[InetSocketAddress, PeerInfo]

  /** banned peer ip -> ban expiration timestamp */
  private var blacklist = Map.empty[InetAddress, TimeProvider.Time]

  /** penalized peer ip -> (accumulated penalty score, last penalty timestamp) */
  private var penaltyBook = Map.empty[InetAddress, (Int, Long)]

  /** Get peer from database */
  override def get(peer: InetSocketAddress): Option[PeerInfo] = peers.get(peer)

  /** Add peer to the database(a Map of InetSocketAddress with PeerInfo) if it's not blacklisted */
  override def addOrUpdateKnownPeer(peerInfo: PeerInfo): Unit =
    if (!peerInfo.metadata.declaredAddress.exists(x => isBlacklisted(x.getAddress))) {
      peerInfo.metadata.address.foreach { address =>
        log.info(s"Updating peer info for $address")
        peers += address -> peerInfo
      }
    }

  /** Blacklist a malicious peer if its penalty score exceeds the penaltyScoreThreshold from network settings */
  override def addToBlacklist(socketAddress: InetSocketAddress, penaltyType: PenaltyType): Unit = {
    peers -= socketAddress
    Option(socketAddress.getAddress).foreach { address =>
      penaltyBook -= address
      if (!blacklist.keySet.contains(address))
        blacklist += address -> (timeProvider.time + penaltyDuration(penaltyType))
      else log.warn(s"${address.toString} is already blacklisted")
    }
  }

  /** Remove a peer from the blacklist(happens if a peer has stayed on the blacklist for specific penalty duration) */
  override def removeFromBlacklist(address: InetAddress): Unit = {
    log.info(s"$address removed from blacklist")
    blacklist -= address
  }

  /** Remove peer from the peer database */
  override def remove(address: InetSocketAddress): Unit =
    peers -= address

  /** @return the peer database(a Map of InetSocketAddress with PeerInfo) */
  override def knownPeers: Map[InetSocketAddress, PeerInfo] = peers

  /** @return a sequence of blacklisted peers */
  override def blacklistedPeers: Seq[InetAddress] = blacklist.map { case (address, bannedTill) =>
    checkBanned(address, bannedTill)
    address
  }.toSeq

  override def isEmpty: Boolean = peers.isEmpty

  /** @return `true` if the peer with corresponding InetAddress is on the blacklist */
  override def isBlacklisted(address: InetAddress): Boolean =
    blacklist.get(address).exists(checkBanned(address, _))

  /** @return `true` if the peer with corresponding InetSocketAddress is on the blacklist */
  def isBlacklisted(address: InetSocketAddress): Boolean =
    Option(address.getAddress).exists(isBlacklisted)

  /**
   * Registers a new penalty in the penalty book.
   *
   * @param socketAddress InetSocketAddress of the peer
   * @param penaltyType type of the penalty
   * @return `true` if penalty threshold is reached, `false` otherwise
   */
  def penalize(socketAddress: InetSocketAddress, penaltyType: PenaltyType): Boolean =
    Option(socketAddress.getAddress).exists { address =>
      val currentTime = timeProvider.time
      val safeInterval = settings.penaltySafeInterval.toMillis
      val (penaltyScoreAcc, lastPenaltyTs) = penaltyBook.getOrElse(address, (0, 0L))
      val applyPenalty = currentTime - lastPenaltyTs - safeInterval > 0 || penaltyType.isPermanent
      val newPenaltyScore =
        if (applyPenalty) penaltyScoreAcc + penaltyScore(penaltyType)
        else penaltyScoreAcc
      if (newPenaltyScore > settings.penaltyScoreThreshold) true
      else {
        penaltyBook += address -> (newPenaltyScore -> timeProvider.time)
        false
      }
    }

  override def peerSeen(peerInfo: PeerInfo): Unit = {
    val pi = peerInfo.copy(lastSeen = timeProvider.time)
    addOrUpdateKnownPeer(pi)
  }

  /**
   * Currently accumulated penalty score for a given address.
   *
   * @param address InetAddress of the peer
   */
  def penaltyScore(address: InetAddress): Int =
    penaltyBook.getOrElse(address, (0, 0L))._1

  def penaltyScore(socketAddress: InetSocketAddress): Int =
    Option(socketAddress.getAddress).map(penaltyScore).getOrElse(0)

  /** @return the penalty score for the given penalty type */
  private def penaltyScore(penaltyType: PenaltyType): Int =
    penaltyType match {
      case PenaltyType.NonDeliveryPenalty =>
        PenaltyType.NonDeliveryPenalty.penaltyScore
      case PenaltyType.MisbehaviorPenalty =>
        PenaltyType.MisbehaviorPenalty.penaltyScore
      case PenaltyType.SpamPenalty =>
        PenaltyType.SpamPenalty.penaltyScore
      case PenaltyType.PermanentPenalty =>
        PenaltyType.PermanentPenalty.penaltyScore
    }

  /** @return `true` if a peer should still be on blacklist */
  private def checkBanned(address: InetAddress, bannedTill: Long): Boolean = {
    val stillBanned = timeProvider.time < bannedTill
    if (!stillBanned) removeFromBlacklist(address)
    stillBanned
  }

  /** @return the penalty duration for the peer that is going on the blacklist */
  private def penaltyDuration(penalty: PenaltyType): Long =
    penalty match {
      case PenaltyType.NonDeliveryPenalty | PenaltyType.MisbehaviorPenalty | PenaltyType.SpamPenalty =>
        settings.temporalBanDuration.toMillis
      case PenaltyType.PermanentPenalty =>
        (360 * 10).days.toMillis
    }
}

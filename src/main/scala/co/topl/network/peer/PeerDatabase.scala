package co.topl.network.peer

import java.net.{InetAddress, InetSocketAddress}

/** Trait for peer databases */
trait PeerDatabase {

  def get(peer: InetSocketAddress): Option[PeerInfo]

  def isEmpty: Boolean

  /** Add peer to the database, or update it
    *
    * @param peerInfo - peer record
    */
  def addOrUpdateKnownPeer(peerInfo: PeerInfo): Unit

  def peerSeen(peerInfo: PeerInfo): Unit

  def knownPeers: Map[InetSocketAddress, PeerInfo]

  def addToBlacklist(address: InetSocketAddress, penaltyType: PenaltyType): Unit

  def removeFromBlacklist(address: InetAddress): Unit

  def blacklistedPeers: Seq[InetAddress]

  def isBlacklisted(address: InetAddress): Boolean

  def remove(address: InetSocketAddress): Unit
}

package bifrost.network.peer

import java.net.InetSocketAddress

import bifrost.settings.Settings

import scala.collection.mutable

case class PeerInfo(lastSeen: Long, nonce: Option[Long] = None, nodeName: Option[String] = None)

//todo: persistence
class PeerDatabase(settings: Settings, filename: Option[String]) {

  private val whitelistPersistence = mutable.Map[InetSocketAddress, PeerInfo]()

  private val blacklist = mutable.Map[String, Long]()

  private lazy val ownNonce = settings.nodeNonce

  def addOrUpdateKnownPeer(address: InetSocketAddress, peerInfo: PeerInfo): Unit = {
    val updatedPeerInfo = whitelistPersistence.get(address).map { dbPeerInfo =>
      val nonceOpt = peerInfo.nonce.orElse(dbPeerInfo.nonce)
      val nodeNameOpt = peerInfo.nodeName.orElse(dbPeerInfo.nodeName)
      PeerInfo(peerInfo.lastSeen, nonceOpt, nodeNameOpt)
    }.getOrElse(peerInfo)
    whitelistPersistence.put(address, updatedPeerInfo)
  }

  def blacklistPeer(address: InetSocketAddress): Unit = {
    whitelistPersistence.remove(address)
    if (!isBlacklisted(address)) blacklist += address.getHostName -> System.currentTimeMillis()
  }

  def isBlacklisted(address: InetSocketAddress): Boolean = {
    blacklist.synchronized(blacklist.contains(address.getHostName))
  }

  def knownPeers(excludeSelf: Boolean): Map[InetSocketAddress, PeerInfo] =
    (excludeSelf match {
      case true => knownPeers(false).filter(_._2.nonce.getOrElse(-1) != ownNonce)
      case false => whitelistPersistence.keys.flatMap(k => whitelistPersistence.get(k).map(v => k -> v))
    }).toMap

  def blacklistedPeers(): Seq[String] = blacklist.keys.toSeq

  def isEmpty(): Boolean = whitelistPersistence.isEmpty
}
package co.topl.network.peer

import co.topl.network.ConnectionDirection
import co.topl.settings.Version

import java.net.InetSocketAddress

/**
 * Information about peer to be stored in PeerDatabase
 *
 * @param metadata general information about the peer
 * @param lastSeen timestamp when this peer was last seen in the network
 * @param connectionType type of connection (Incoming/Outgoing) established to this peer if any
 */
case class PeerInfo(metadata: PeerMetadata, lastSeen: Long, connectionType: Option[ConnectionDirection] = None)

object PeerInfo {

  /**
   * Create peer info from address only, when we don't know other fields
   * (e.g. we got this information from config or from API)
   */
  def fromAddress(address: InetSocketAddress): PeerInfo = {
    val metadata = PeerMetadata("unknown", Version.initial, s"unknown-$address", Some(address), Seq())
    PeerInfo(metadata, 0L, None)
  }
}

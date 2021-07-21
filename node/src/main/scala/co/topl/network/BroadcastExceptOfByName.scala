package co.topl.network

import co.topl.network.peer.ConnectedPeer

case class BroadcastExceptOfByName(exceptOf: String) extends SendingStrategy {
  override def choose(peers: Seq[ConnectedPeer]): Seq[ConnectedPeer] =
    peers.filterNot(p => p.peerInfo.get.peerSpec.agentName == exceptOf)
}

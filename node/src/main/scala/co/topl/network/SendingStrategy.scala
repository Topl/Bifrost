package co.topl.network

import co.topl.network.peer.ConnectedPeer

import scala.util.Random

/** Sending strategies used by NodeViewSynchronizer and PeerSynchronizer to send messages to peers */
trait SendingStrategy {
  def choose(peers: Seq[ConnectedPeer]): Seq[ConnectedPeer]
}

object SendToRandom extends SendingStrategy {

  override def choose(peers: Seq[ConnectedPeer]): Seq[ConnectedPeer] =
    if (peers.nonEmpty) {
      Seq(peers(Random.nextInt(peers.length)))
    } else {
      Seq.empty
    }
}

case object Broadcast extends SendingStrategy {
  override def choose(peers: Seq[ConnectedPeer]): Seq[ConnectedPeer] = peers
}

case class BroadcastExceptOf(exceptOf: Seq[ConnectedPeer]) extends SendingStrategy {

  override def choose(peers: Seq[ConnectedPeer]): Seq[ConnectedPeer] =
    peers.filterNot(exceptOf.contains)
}

case class SendToPeer(chosenPeer: ConnectedPeer) extends SendingStrategy {
  override def choose(peers: Seq[ConnectedPeer]): Seq[ConnectedPeer] = Seq(chosenPeer)
}

case class SendToPeers(chosenPeers: Seq[ConnectedPeer]) extends SendingStrategy {
  override def choose(peers: Seq[ConnectedPeer]): Seq[ConnectedPeer] = chosenPeers
}

case class SendToRandomFromChosen(chosenPeers: Seq[ConnectedPeer]) extends SendingStrategy {

  override def choose(peers: Seq[ConnectedPeer]): Seq[ConnectedPeer] =
    Seq(chosenPeers(Random.nextInt(chosenPeers.length)))
}

package co.topl.networking.fsnetwork

import scala.annotation.tailrec
import scala.util.Random

abstract class SelectorColdToWarm[F[_]] {
  def select(hosts: Map[HostId, Peer[F]], countToReceive: Int): Set[HostId]
}

class SemiRandomSelectorColdToWarm[F[_]] extends SelectorColdToWarm[F] {

  @tailrec
  final override def select(hosts: Map[HostId, Peer[F]], countToReceive: Int): Set[HostId] = {
    val randomPeers: Seq[HostId] =
      Random.shuffle(hosts).take(countToReceive).keys.toSeq

    val reputationPeers: Seq[HostId] =
      hosts.toSeq
        .sortBy { case (_, peer) => peer.blockRep + peer.perfRep }
        .takeRight(countToReceive)
        .map(_._1)

    // take double reputation peers according to simulation
    val res =
      Random.shuffle(randomPeers ++ reputationPeers ++ reputationPeers).take(countToReceive).toSet

    if (res.size == countToReceive || res.size == hosts.size) {
      res
    } else {
      select(hosts, countToReceive)
    }
  }

}

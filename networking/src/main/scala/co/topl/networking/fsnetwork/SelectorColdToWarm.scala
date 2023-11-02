package co.topl.networking.fsnetwork

import co.topl.networking.p2p.RemoteAddress

import scala.annotation.tailrec
import scala.util.Random

abstract class SelectorColdToWarm[F[_]] {
  def select(hosts: Set[Peer[F]], countToReceive: Int): Set[RemoteAddress]
}

class SemiRandomSelectorColdToWarm[F[_]] extends SelectorColdToWarm[F] {

  @tailrec
  final def select(hosts: Set[Peer[F]], countToReceive: Int): Set[RemoteAddress] = {
    val randomPeers: Seq[RemoteAddress] =
      Random.shuffle(hosts).take(countToReceive).flatMap(_.asRemoteAddress).toSeq

    val reputationPeers: Seq[RemoteAddress] =
      hosts.toSeq
        .sortBy(peer => peer.blockRep + peer.perfRep)
        .takeRight(countToReceive)
        .flatMap(_.asRemoteAddress)

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

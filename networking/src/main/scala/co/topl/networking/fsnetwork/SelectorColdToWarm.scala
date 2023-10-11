package co.topl.networking.fsnetwork

import co.topl.networking.p2p.RemoteAddress

import scala.util.Random

abstract class SelectorColdToWarm[F[_]] {
  def select(hosts: Set[Peer[F]], countToReceive: Int): Set[RemoteAddress]
}

class SemiRandomSelectorColdToWarm[F[_]](closeTimeoutFirstDelayInMs: Long) extends SelectorColdToWarm[F] {

  def select(hosts: Set[Peer[F]], countToReceive: Int): Set[RemoteAddress] = {
    val currentTimestamp = System.currentTimeMillis()

    val eligibleColdPeers = hosts.filter { p =>
      val timestamps = p.closedTimestamps
      val lastClose = timestamps.lastOption.getOrElse(0L)
      val totalCloses = timestamps.size
      val nonEligibleWindow = totalCloses * totalCloses * closeTimeoutFirstDelayInMs
      currentTimestamp.toDouble >= (lastClose + nonEligibleWindow)
    }

    val randomPeers: Set[RemoteAddress] =
      Random.shuffle(eligibleColdPeers).take(countToReceive).flatMap(_.asRemoteAddress)

    val reputationPeers: Set[RemoteAddress] =
      eligibleColdPeers.toSeq
        .sortBy(peer => (peer.lastKnownBlockProvidingReputation + peer.lastKnownPerformanceReputation))
        .takeRight(countToReceive)
        .flatMap(_.asRemoteAddress)
        .toSet

    Random.shuffle(randomPeers ++ reputationPeers).take(countToReceive)
  }

}

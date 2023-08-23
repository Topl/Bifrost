package co.topl.networking.fsnetwork

import co.topl.networking.p2p.RemoteAddress

import scala.util.Random

abstract class ColdToWarmSelector[F[_]] {
  def select(hosts: Set[PeerWithHostAndPort[F]], countToReceive: Int): Set[RemoteAddress]
}

class RandomColdToWarmSelector[F[_]](closeTimeoutFirstDelayInMs: Long) extends ColdToWarmSelector[F] {

  def select(hosts: Set[PeerWithHostAndPort[F]], countToReceive: Int): Set[RemoteAddress] = {
    val currentTimestamp = System.currentTimeMillis()

    val eligibleColdPeers = hosts.filter { p =>
      val timestamps = p.closeTimestamps
      val lastClose = timestamps.lastOption.getOrElse(0L)
      val totalCloses = timestamps.size
      val nonEligibleWindow = totalCloses * totalCloses * closeTimeoutFirstDelayInMs
      currentTimestamp.toDouble >= (lastClose + nonEligibleWindow)
    }

    Random.shuffle(eligibleColdPeers).take(countToReceive).map(_.asRemoteAddress)
  }
}

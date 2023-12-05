package co.topl.networking.fsnetwork

import scala.util.Random

abstract class SelectorWarmToHot[F[_]] {
  def select(hosts: Map[HostId, Peer[F]], countToReceive: Int): Set[HostId]
}

class ReputationRandomBasedSelectorWarmToHot[F[_]] extends SelectorWarmToHot[F] {

  def select(hosts: Map[HostId, Peer[F]], countToReceive: Int): Set[HostId] = {
    val random: Seq[HostId] =
      Random.shuffle(hosts.keySet).take(countToReceive).toSeq

    val perfReputation: Seq[HostId] =
      hosts.toSeq.sortBy(_._2.perfRep).takeRight(countToReceive).map(_._1)

    val blockReputation: Seq[HostId] =
      hosts.toSeq.sortBy(_._2.blockRep).takeRight(countToReceive).map(_._1)

    Random.shuffle(random ++ perfReputation ++ blockReputation).take(countToReceive).toSet
  }

}

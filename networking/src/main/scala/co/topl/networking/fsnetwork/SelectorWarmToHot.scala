package co.topl.networking.fsnetwork

import co.topl.networking.p2p.RemoteAddress
import scala.util.Random

abstract class SelectorWarmToHot[F[_]] {
  def select(hosts: Set[Peer[F]], countToReceive: Int): Set[RemoteAddress]
}

class ReputationRandomBasedSelectorWarmToHot[F[_]] extends SelectorWarmToHot[F] {

  def select(hosts: Set[Peer[F]], countToReceive: Int): Set[RemoteAddress] = {
    val random: Seq[RemoteAddress] =
      Random.shuffle(hosts.flatMap(_.asRemoteAddress)).take(countToReceive).toSeq

    val perfReputation: Seq[RemoteAddress] =
      hosts.toSeq.sortBy(_.perfRep).takeRight(countToReceive).flatMap(_.asRemoteAddress)

    val blockReputation: Seq[RemoteAddress] =
      hosts.toSeq.sortBy(_.blockRep).takeRight(countToReceive).flatMap(_.asRemoteAddress)

    Random.shuffle(random ++ perfReputation ++ blockReputation).take(countToReceive).toSet
  }

}

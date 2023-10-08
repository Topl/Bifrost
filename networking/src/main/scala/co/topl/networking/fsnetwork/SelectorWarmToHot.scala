package co.topl.networking.fsnetwork

import co.topl.networking.p2p.RemoteAddress
import scala.util.Random

abstract class SelectorWarmToHot[F[_]] {
  def select(hosts: Set[Peer[F]], countToReceive: Int): Set[RemoteAddress]
}

class ReputationBasedSelectorWarmToHot[F[_]] extends SelectorWarmToHot[F] {

  def select(hosts: Set[Peer[F]], countToReceive: Int): Set[RemoteAddress] = {
    val random: Set[RemoteAddress] =
      Random.shuffle(hosts.flatMap(_.asRemoteAddress)).take(countToReceive)

    val reputation: Set[RemoteAddress] =
      hosts.toSeq.sortBy(_.overallReputation).takeRight(countToReceive).flatMap(_.asRemoteAddress).toSet

    Random.shuffle(random ++ reputation).take(countToReceive)
  }

}

package co.topl.settings

import scala.collection.SortedSet
import scala.concurrent.duration.FiniteDuration

sealed abstract class ProtocolRules extends Ordered[ProtocolRules] {
  val versionLimit: Version
  val blockHeightLimit: Long

  final protected def compare (that: ProtocolRules): Int = {
    if (this.versionLimit == that.versionLimit) {
      0
    } else if (this.versionLimit > that.versionLimit) {
      1
    } else {
      -1
    }
  }
}

object ProtocolRules {
  /** Defines the list of all different rule updates during the lifetime of the protocol */
  private def all: SortedSet[ProtocolRules] = SortedSet(Monon_0, Monon_1)

  /**
   * Find the set of protocol rules that apply to this version of the software
   */
  private def applicable(appVersion: Version): SortedSet[ProtocolRules] =
    all.takeWhile(appVersion >= _.versionLimit)

  /**
   * Finds the currently applicable set of consensus rules
   * @param appVersion
   * @param blockHeight
   * @return
   */
  def current(appVersion: Version)(blockHeight: Long): Option[ProtocolRules] =
    applicable(appVersion).find(blockHeight <= _.blockHeightLimit)
}

case class Monon_0( versionLimit: Version,
                    blockHeightLimit: Long,
                    targetBlockTime: FiniteDuration,
                    numTxPerBlock: Int,
                    nxtBlockNum: Int) extends ProtocolRules

case class Monon_1( versionLimit: Version,
                    blockHeightLimit: Long,
                    targetBlockTime: FiniteDuration,
                    numTxPerBlock: Int,
                    nxtBlockNum: Int) extends ProtocolRules

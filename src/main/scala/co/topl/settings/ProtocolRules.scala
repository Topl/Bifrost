package co.topl.settings

import scala.collection.SortedSet
import scala.concurrent.duration.FiniteDuration

sealed abstract class ProtocolRules extends Ordered[ProtocolRules] {
  // The default values should really only apply to the latest ruleset
  // (since it is confusing to define limits that are unknown).
  // versionLimit is set to the minimum value since the latest version should be included in the applicable test
  // blockLimit is set to the max value since we don't want to switch versions from the latest version
  lazy val versionLimit: Version = Version.initial
  lazy val blockHeightLimit: Long = Long.MaxValue

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

/**
  * The case class  below should be used to define new versions of the protocol rule set.
  * The parameters changed here must be agreed upon since they are used in the evaluation of
  * the consensus mechanism.
  */
case class Monon_0( override val versionLimit: Version,
                    override val blockHeightLimit: Long,
                    targetBlockTime: FiniteDuration,
                    numTxPerBlock: Int,
                    nxtBlockNum: Int) extends ProtocolRules

case class Monon_1( targetBlockTime: FiniteDuration,
                    numTxPerBlock: Int,
                    nxtBlockNum: Int) extends ProtocolRules

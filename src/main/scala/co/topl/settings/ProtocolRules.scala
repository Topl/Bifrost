package co.topl.settings

import scala.concurrent.duration.FiniteDuration

sealed trait ProtocolRules extends Ordered[ProtocolRules] {
  // The default values should really only apply to the latest ruleset
  // (since it is confusing to define limits that are unknown).
  // versionLimit is set to the max value since the latest version should be included in the applicable test
  // blockLimit is set to the max value since we don't want to switch versions from the latest version
  val versionLimit: Version = Version.MaxValue
  val blockHeightLimit: Long = Long.MaxValue
  val targetBlockTime: FiniteDuration
  val numTxPerBlock: Int
  val blockVersion: Byte


  def compare (that: ProtocolRules): Int = {
    if (this.versionLimit == that.versionLimit) {
      0
    } else if (this.versionLimit > that.versionLimit) {
      1
    } else {
      -1
    }
  }

  def equals (that: ProtocolRules): Boolean = {
    this.versionLimit == that.versionLimit
  }
}

/**
  * The case class below should be used to define new versions of the protocol rule set.
  * The parameters changed here must be agreed upon since they are used in the evaluation of
  * the consensus mechanism.
  */
case class Monon_0( override val versionLimit: Version,
                    override val blockHeightLimit: Long,
                    targetBlockTime: FiniteDuration,
                    numTxPerBlock: Int,
                    blockVersion: Byte) extends ProtocolRules

case class Monon_01( override val versionLimit: Version,
                    override val blockHeightLimit: Long,
                    targetBlockTime: FiniteDuration,
                    numTxPerBlock: Int,
                    blockVersion: Byte) extends ProtocolRules

case class Monon_1( targetBlockTime: FiniteDuration,
                    numTxPerBlock: Int,
                    blockVersion: Byte) extends ProtocolRules

package co.topl.settings

import scala.concurrent.duration.FiniteDuration

sealed trait ProtocolRules extends Ordered[ProtocolRules] {
  // blockLimit is set to the max value since the latest protocol version should apply indefinitely
  val blockHeightLimit: Long = Long.MaxValue
  val targetBlockTime: FiniteDuration
  val numTxPerBlock: Int
  val blockVersion: Byte

  def compare (that: ProtocolRules): Int =
    this.blockHeightLimit compare that.blockHeightLimit

  def equals (that: ProtocolRules): Boolean =
    this.blockHeightLimit == that.blockHeightLimit
}

/**
 * The case class below should be used to define new versions of the protocol rule set.
 * The parameters changed here must be agreed upon since they are used in the evaluation of
 * the consensus mechanism.
 */
case class Monon_0( override val blockHeightLimit: Long,
                    targetBlockTime: FiniteDuration,
                    numTxPerBlock: Int,
                    blockVersion: Byte) extends ProtocolRules

case class Monon_01( override val blockHeightLimit: Long,
                     targetBlockTime: FiniteDuration,
                     numTxPerBlock: Int,
                     blockVersion: Byte) extends ProtocolRules

case class Monon_1( targetBlockTime: FiniteDuration,
                    numTxPerBlock: Int,
                    blockVersion: Byte) extends ProtocolRules

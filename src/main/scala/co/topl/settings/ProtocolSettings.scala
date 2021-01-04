package co.topl.settings

import scala.concurrent.duration.FiniteDuration

/**
 * The case class below should be used to define new versions of the protocol rule set.
 * The parameters changed here must be agreed upon since they are used in the evaluation of
 * the consensus mechanism.
 *
 * versionLimit and blockLimit is set to the max value since the latest protocol version should apply indefinitely
 */
case class ProtocolSettings ( version        : Version,
                              startBlock     : Long,
                              targetBlockTime: Option[FiniteDuration] = None,
                              numTxPerBlock  : Option[Int] = None,
                              blockVersion   : Option[Byte] = None
                            ) extends Ordered[ProtocolSettings] {

  // want reverse ordering such that the highest start block is first in the list
  // so that traversing the sortedSet will find the first applicable settings
  def compare (that: ProtocolSettings): Int =
    -1 * (this.startBlock compare that.startBlock)

  def equals (that: ProtocolSettings): Boolean =
    this.startBlock == that.startBlock
}
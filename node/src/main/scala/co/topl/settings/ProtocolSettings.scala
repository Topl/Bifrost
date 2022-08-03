package co.topl.settings

import scala.concurrent.duration.{DurationInt, FiniteDuration}

/**
 * The case class below should be used to define new versions of the protocol rule set.
 * The parameters changed here must be agreed upon since they are used in the evaluation of
 * the consensus mechanism.
 *
 * versionLimit and blockLimit is set to the max value since the latest protocol version should apply indefinitely
 *
 * @param minAppVersion minimum applicable software version for these protocol settings
 * @param startBlock starting block height for the protocol settings
 *
 * @param blockVersion applicable block serializer version
 */
case class ProtocolSettings(
  minAppVersion: Version,
  startBlock:    Long,
  blockVersion:  Byte,
  value:         ProtocolConfigurations.Dion // JAA - we may want to abstract this but concretely specifying for now
) extends Ordered[ProtocolSettings] {

  /**
   * Want reverse ordering such that the highest start block is first in the list so that traversing the sortedSet
   * will find the first applicable settings
   */
  def compare(that: ProtocolSettings): Int = -1 * (this.startBlock compare that.startBlock)

  override def equals(obj: Any): Boolean = obj match {
    case ps: ProtocolSettings => this.startBlock == ps.startBlock
    case _                    => false
  }

  override def hashCode(): Int = (this.startBlock % Int.MaxValue).toInt
}

object ProtocolSettings {

  val default: ProtocolSettings =
    ProtocolSettings(
      minAppVersion = Version.initial,
      startBlock = 0L,
      blockVersion = 1,
      value = ProtocolConfigurations.Dion(
        targetBlockTime = 3.second,
        numTxPerBlock = 100,
        inflationRate = 0,
        lookBackDepth = 3
      )
    )
}

/**
 * Protocol configurations encapsulate the different types of protocol values which directly effect the consensus mechanism.
 * Changing the value of these parameters should be considered a hard-fork as nodes that adopt new parameters will be unable
 * to exchange data with nodes that do not adopt any parameter changes.
 */
sealed trait ProtocolConfiguration

object ProtocolConfigurations {

  /**
   * The Dion protocol parameters
   * @param targetBlockTime Agreed upon block time target
   * @param numTxPerBlock number of transactions per block
   * @param inflationRate the number of arbits that would be created each block
   * @param lookBackDepth the number of blocks to use for averaging the timestamps and adjusting difficulty
   */
  case class Dion(targetBlockTime: FiniteDuration, numTxPerBlock: Int, inflationRate: Int, lookBackDepth: Int)
      extends ProtocolConfiguration
}

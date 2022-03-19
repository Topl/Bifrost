package co.topl.consensus

import co.topl.settings.{ProtocolSettings, Version}
import co.topl.utils.Int128

import scala.collection.SortedSet
import scala.concurrent.duration.FiniteDuration

/**
 * This class provides functionality for managing the backwards compatibility with previous
 * protocol definitions
 * @param protocolVersions all previous rule sets (that must be available in the configuration file)
 */
class ProtocolVersioner private (appVersion: Version, protocolVersions: SortedSet[ProtocolSettings]) {

  /** this is the set of protocol settings a particular version of the software can utilize */
  lazy val applicable: SortedSet[ProtocolSettings] = protocolVersions.filter(appVersion >= _.version)

  /**
   * Finds the consensus rules that should be used based on block height
   * @param blockHeight height of the block being considered
   * @return
   */
  def current(blockHeight: Int128): Option[ProtocolSettings] = applicable.find(blockHeight >= _.startBlock)

  /** Find the rule set for the given app version and block height */
  def getProtocolRules(blockHeight: Long): ProtocolSettings =
    current(blockHeight)
      .getOrElse(throw new Error("Unable to find applicable protocol rules"))

  def targetBlockTime(blockHeight: Long): FiniteDuration =
    getProtocolRules(blockHeight).targetBlockTime.get

  def numTxInBlock(blockHeight: Long): Int =
    getProtocolRules(blockHeight).numTxPerBlock.get

  def blockVersion(blockHeight: Long): Byte =
    getProtocolRules(blockHeight).blockVersion.get
}

object ProtocolVersioner {

  def apply(appVersion: Version, protocolVersions: Seq[ProtocolSettings]): ProtocolVersioner = {
    val sortedAndUniqueVersions = SortedSet[ProtocolSettings]() ++ protocolVersions.toSet
    require(sortedAndUniqueVersions.size == protocolVersions.size, "Non-unique protocol versions specified at runtime")
    new ProtocolVersioner(appVersion, sortedAndUniqueVersions)
  }

  def empty: ProtocolVersioner = ProtocolVersioner(Version.initial, Seq())
}

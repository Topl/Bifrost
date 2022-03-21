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
  lazy val compatibleProtocolVersions: SortedSet[ProtocolSettings] = protocolVersions.filter(appVersion >= _.minAppVersion)

  /**
   * Finds the consensus rules that should be used based on block height and appVersion
   * @param blockHeight height of the block being considered
   * @return
   */
  def applicable(blockHeight: Long): ProtocolSettings =
    compatibleProtocolVersions.find(blockHeight >= _.startBlock)
      .getOrElse(throw new Error("Unable to find applicable protocol rules"))
}

object ProtocolVersioner {

  lazy val default: ProtocolVersioner = ProtocolVersioner(Version.initial, Seq(ProtocolSettings.default))

  def apply(appVersion: Version, protocolVersions: Seq[ProtocolSettings]): ProtocolVersioner = {
    val sortedAndUniqueVersions = SortedSet[ProtocolSettings]() ++ protocolVersions.toSet
    require(sortedAndUniqueVersions.size == protocolVersions.size, "Non-unique protocol versions specified at runtime")
    new ProtocolVersioner(appVersion, sortedAndUniqueVersions)
  }
}

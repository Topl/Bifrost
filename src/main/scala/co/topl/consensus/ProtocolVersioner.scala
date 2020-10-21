package co.topl.consensus

import co.topl.settings.ProtocolRules

import scala.collection.SortedSet

/**
 * This class provides functionality for managing the backwards compatibility with previous
 * protocol definitions
 * @param protocolVersions all previous rule sets (that must be available in the configuration file)
 */
class ProtocolVersioner(protocolVersions: SortedSet[ProtocolRules]) {
  /**
   * Finds the currently applicable set of consensus rules based on block height
   * @param blockHeight height of the block being considered
   * @return
   */
  def current(blockHeight: Long): Option[ProtocolRules] =
    protocolVersions.find(blockHeight <= _.blockHeightLimit)
}

object ProtocolVersioner {
  def apply(protocolVersions: Seq[ProtocolRules]): ProtocolVersioner = {
    val sortedAndUniqueVersions = SortedSet[ProtocolRules]() ++ protocolVersions.toSet
    require(sortedAndUniqueVersions.size == protocolVersions.size, "Non-unique protocol versions specified at runtime")
    new ProtocolVersioner(sortedAndUniqueVersions)
  }

  def empty: ProtocolVersioner = ProtocolVersioner(Seq())
}

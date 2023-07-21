package co.topl.node

import cats.{Eq, Order}
import scala.collection.SortedSet
import cats.syntax.all._
import co.topl.config.ApplicationConfig.Bifrost
import co.topl.consensus.models.ProtocolVersion
import co.topl.models.Slot
import scala.util.Try

/**
 * This class provides functionality for managing the backwards compatibility with previous protocol definitions
 * @param appVersion application version
 * @param protocolVersions all previous rule sets (that must be available in the configuration file)
 */
case class ProtocolVersioner(appVersion: Version, protocolVersions: SortedSet[ProtocolSettings]) {

  /** this is the set of protocol settings a particular version of the software can utilize */
  lazy val compatibleProtocolVersions: SortedSet[ProtocolSettings] =
    protocolVersions.filter(appVersion >= _.minAppVersion)

  /**
   * Finds the consensus rules that should be used based on block height and appVersion
   *
   * @param blockHeight height of the block being considered
   * @return
   */
  def applicable(blockHeight: Long): ProtocolSettings =
    compatibleProtocolVersions
      .find(pv => blockHeight >= pv.startBlock)
      .getOrElse(throw new Error("Unable to find applicable protocol rules"))
}

object ProtocolVersioner {

  def apply(protocols: Map[Slot, Bifrost.Protocol]): ProtocolVersioner = {
    val setProtocols = protocols.map { case (slot, protocol) =>
      ProtocolSettings(Version(protocol.minAppVersion), slot, protocol)
    }.toSeq
    val protocolVersions = SortedSet.from(setProtocols)(ProtocolSettings.orderProtocolSettings.toOrdering)
    ProtocolVersioner(Version.DefaultVersion, protocolVersions)
  }
}

case class Version(firstDigit: Int, secondDigit: Int, thirdDigit: Int)

object Version {

  val DefaultVersion = Version(2, 0, 0)

  // eq and order Version could not live on type-classes because Bifrost.Protocol are not in models.
  implicit val eqVersion: Eq[Version] =
    (a, b) =>
      a.firstDigit === b.firstDigit &&
      a.secondDigit === b.secondDigit &&
      a.thirdDigit === b.thirdDigit

  implicit val orderVersion: Order[Version] =
    Order.from[Version] { (x, y) =>
      if (x.firstDigit != y.firstDigit) {
        x.firstDigit - y.firstDigit
      } else if (x.secondDigit != y.secondDigit) {
        x.secondDigit - y.secondDigit
      } else {
        x.thirdDigit - y.thirdDigit
      }
    }

  def apply(value: String): Version =
    Try(value.split("\\."))
      .flatMap(split => Try(Version(split(0).toInt, split(1).toInt, split(2).toInt)))
      .getOrElse(DefaultVersion)

  implicit class VersionOps(v: Version) {
    def asProtocolVersion: ProtocolVersion = ProtocolVersion(v.firstDigit, v.secondDigit, v.thirdDigit)
  }
}

/**
 * @param minAppVersion minimum applicable software version for these protocol settings
 * @param startBlock  starting block height for the protocol settings
 * @param settings config settings
 */
case class ProtocolSettings(minAppVersion: Version, startBlock: Long, settings: Bifrost.Protocol)

object ProtocolSettings {

  // Want reverse ordering such that the highest start block is first in the list so that traversing the sortedSet will find the first applicable settings
  implicit val orderProtocolSettings: Order[ProtocolSettings] =
    Order.from[ProtocolSettings] { (x, y) =>
      -1 * x.startBlock.compare(y.startBlock)
    }

}

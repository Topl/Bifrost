package co.topl.utils

import co.topl.utils.NetworkType.NetworkPrefix

/**
 * Attributes of a network type such as its name and whether to start forging once it's ready
 * @param verboseName name of the network type
 * @param netPrefix byte that represents the network type
 */
sealed abstract class NetworkType(val verboseName: String, val netPrefix: NetworkPrefix)

object NetworkType {

  type NetworkPrefix = Byte

  lazy val all: Seq[NetworkType] = Seq(Mainnet, ValhallaTestnet, HelTestnet, PrivateTestnet)

  def pickNetworkType(name:          String): Option[NetworkType] = all.find(_.verboseName == name)
  def pickNetworkType(networkPrefix: NetworkPrefix): Option[NetworkType] = all.find(_.netPrefix == networkPrefix)

  case object Mainnet extends NetworkType("toplnet", 1.toByte)

  case object ValhallaTestnet extends NetworkType("valhalla", 16.toByte)

  case object HelTestnet extends NetworkType("hel", 32.toByte)

  case object PrivateTestnet extends NetworkType("private", 64.toByte)

}

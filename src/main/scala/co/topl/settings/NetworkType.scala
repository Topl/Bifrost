package co.topl.settings

import co.topl.attestation.AddressEncoder.NetworkPrefix

/** Attributes of a network type such as its name and whether to start forging once it's ready
  * @param verboseName name of the network type
  * @param netPrefix byte that represents the network type
  */
sealed abstract class NetworkType(val verboseName: String, val netPrefix: NetworkPrefix)

object NetworkType {

  lazy val all: Seq[NetworkType] = Seq(MainNet, TestNet, DevNet, LocalNet, PrivateNet)

  def pickNetworkType(name: String): Option[NetworkType] = all.find(_.verboseName == name)
  def pickNetworkType(networkPrefix: NetworkPrefix): Option[NetworkType] = all.find(_.netPrefix == networkPrefix)

  case object MainNet
    extends NetworkType("toplnet", 1.toByte)

  case object TestNet
    extends NetworkType("valhalla", 16.toByte)

  case object DevNet
    extends NetworkType("hel", 32.toByte)

  case object LocalNet
    extends NetworkType("local", 48.toByte)

  case object PrivateNet
    extends NetworkType("private", 64.toByte)
}

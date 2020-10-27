package co.topl.settings

sealed abstract class NetworkType(val verboseName    : String,
                                  val isMainNet      : Boolean = false,
                                  val isPrivateForger: Boolean = false )

object NetworkType {

  lazy val all: Seq[NetworkType] = Seq(MainNet, TestNet, DevNet, LocalNet, PrivateNet(StartupOpts.empty))

  def fromString(name: String): Option[NetworkType] = all.find(_.verboseName == name)

  case object MainNet extends NetworkType("toplnet", true)
  case object TestNet extends NetworkType("valhalla")
  case object DevNet extends NetworkType("hel")
  case object LocalNet extends NetworkType("local")
  case class PrivateNet(startupOpts: StartupOpts) extends NetworkType("private" , isPrivateForger = true)

}

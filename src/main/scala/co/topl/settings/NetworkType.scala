package co.topl.settings

sealed abstract class NetworkType {
  val verboseName: String
  def isMainNet: Boolean = false
  def isPrivateForger: Boolean = false
}

object NetworkType {

  lazy val all: Seq[NetworkType] = Seq(MainNet, TestNet, DevNet, LocalNet, PrivateNet)

  def fromString(name: String): Option[NetworkType] = all.find(_.verboseName == name)

  case object MainNet extends NetworkType {
    val verboseName: String = "toplnet"
    override def isMainNet: Boolean = true
  }

  case object TestNet extends NetworkType {
    val verboseName: String = "valhalla"
  }

  case object DevNet extends NetworkType {
    val verboseName: String = "hel"
  }

  case object LocalNet extends NetworkType {
    val verboseName: String = "local"
  }

  case object PrivateNet extends NetworkType {
    val verboseName: String = "private"
    override def isPrivateForger = true
  }
}

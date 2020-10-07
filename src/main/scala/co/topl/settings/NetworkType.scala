package co.topl.settings

sealed trait NetworkType {
  val verboseName: String
  def isMainNet: Boolean = false
}

object NetworkType {

  def all: Seq[NetworkType] = Seq(MainNet, TestNet, DevNet)

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

}

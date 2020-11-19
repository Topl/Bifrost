package co.topl.settings

/** Attributes of a network type such as its name and whether to start forging once it's ready
  * @param verboseName name of the network type
  * @param startWithForging is true if the node should start forging when it's ready
  */
sealed abstract class NetworkType(val verboseName: String, val startWithForging: Boolean = false)

object NetworkType {

  lazy val all: Seq[NetworkType] = Seq(MainNet(), TestNet(), DevNet(), LocalNet(), PrivateNet())

  def fromString(name: String): Option[NetworkType] = all.find(_.verboseName == name)

  /** Creates a usable instance of the network type during application initialization
    *
    * @param net the specified network type from the command line
    * @param opts runtime parameters used to control the behavior of the chosen entwork type
    * @return NetworkType case class
    */
  def fillNetworkType(net: NetworkType, opts: RuntimeOpts): NetworkType = net match {
    case MainNet(_)    => MainNet(opts)
    case TestNet(_)    => TestNet(opts)
    case DevNet(_)     => DevNet(opts)
    case LocalNet(_)   => LocalNet(opts)
    case PrivateNet(_) => PrivateNet(opts)
  }

  case class MainNet(opts: RuntimeOpts = RuntimeOpts.empty)
      extends NetworkType("toplnet", startWithForging = opts.startWithForging)

  case class TestNet(opts: RuntimeOpts = RuntimeOpts.empty)
      extends NetworkType("valhalla", startWithForging = opts.startWithForging)

  case class DevNet(opts: RuntimeOpts = RuntimeOpts.empty)
      extends NetworkType("hel", startWithForging = opts.startWithForging)

  case class LocalNet(opts: RuntimeOpts = RuntimeOpts.empty)
      extends NetworkType("local", startWithForging = opts.startWithForging)
  case class PrivateNet(opts: RuntimeOpts = RuntimeOpts.empty) extends NetworkType("private", startWithForging = true)
}

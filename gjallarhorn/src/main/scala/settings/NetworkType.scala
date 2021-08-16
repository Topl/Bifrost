package settings

sealed abstract class NetworkType(
  val verboseName:      String,
  val netPrefix:        NetworkPrefix,
  val startWithForging: Boolean = false
)

object NetworkType {

  lazy val all: Seq[NetworkType] = Seq(MainNet(), TestNet(), DevNet(), LocalNet(), PrivateNet())

  def fromString(name: String): Option[NetworkType] = all.find(_.verboseName == name)

  def fromPrefix(prefix: NetworkPrefix): Option[NetworkType] = all.find(_.netPrefix == prefix)

  /**
   * Creates a usable instance of the network type during application initialization
   *
   * @param net the specified network type from the command line
   * @param opts runtime parameters used to control the behavior of the chosen entwork type
   * @return
   */
  def fillNetworkType(net: NetworkType, opts: RuntimeOpts): NetworkType = net match {
    case MainNet(_)    => MainNet(opts)
    case TestNet(_)    => TestNet(opts)
    case DevNet(_)     => DevNet(opts)
    case LocalNet(_)   => LocalNet(opts)
    case PrivateNet(_) => PrivateNet(opts)
  }

  case class MainNet(opts: RuntimeOpts = RuntimeOpts.empty)
      extends NetworkType("toplnet", 1.toByte, startWithForging = opts.startWithForging)

  case class TestNet(opts: RuntimeOpts = RuntimeOpts.empty)
      extends NetworkType("valhalla", 16.toByte, startWithForging = opts.startWithForging)

  case class DevNet(opts: RuntimeOpts = RuntimeOpts.empty)
      extends NetworkType("hel", 32.toByte, startWithForging = opts.startWithForging)

  case class LocalNet(opts: RuntimeOpts = RuntimeOpts.empty)
      extends NetworkType("local", 48.toByte, startWithForging = opts.startWithForging)

  case class PrivateNet(opts: RuntimeOpts = RuntimeOpts.empty)
      extends NetworkType("private", 64.toByte, startWithForging = true)
}

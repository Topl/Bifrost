package co.topl.settings

final case class StartupOpts(userConfigPathOpt: Option[String],
                      networkTypeOpt: Option[NetworkType])

object StartupOpts {
  def empty: StartupOpts = StartupOpts(None, None)
}

package co.topl.settings

final case class StartupOpts( userConfigPathOpt: Option[String],
                              networkTypeOpt: Option[NetworkType],
                              seed: Option[String]
                            )

object StartupOpts {
  def empty: StartupOpts = StartupOpts(None, None, None)
}

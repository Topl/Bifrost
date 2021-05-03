package settings

/**
 * Parameters that are given at application startup. Only parameters that are
 * required for initialization should be included at the top level while all other
 * settings that control the application runtime (similar in nature to config settings)
 * are delegated to the runtime options
 *
 * @param userConfigPathOpt file path to the user defined config file
 * @param networkTypeOpt string designating the type of network to be launched
 * @param runtimeParams all runtime determined application settings
 */
final case class StartupOpts(
  userConfigPathOpt: Option[String] = None,
  networkTypeOpt:    Option[NetworkType] = None,
  runtimeParams:     RuntimeOpts = RuntimeOpts.empty
)

object StartupOpts {
  // used defaults above to simplify the empty
  def empty: StartupOpts = new StartupOpts()
}

/* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */

/**
 * Parameters that control the application behavior that are specified at runtime.
 * These settings could just as well be defined in the config file but may be
 * useful to define them as command line parameters instead.
 *
 * @param seed a string used to deterministically generate key files (only applicable for local and private networks)
 * @param startWithForging a boolean controlling whether the node should attempt forging immediately on start
 */
final case class RuntimeOpts(seed: Option[String] = None, startWithForging: Boolean = false)

object RuntimeOpts {
  def empty: RuntimeOpts = RuntimeOpts()
}

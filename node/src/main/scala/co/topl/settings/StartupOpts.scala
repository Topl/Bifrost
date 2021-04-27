package co.topl.settings

import co.topl.utils.NetworkType
import mainargs.{Flag, arg, main}

/** Parameters that are given at application startup. Only parameters that are
  * required for initialization should be included at the top level while all other
  * settings that control the application runtime (similar in nature to config settings)
  * are delegated to the runtime options
  *
  * @param userConfigPathOpt file path to the user defined config file
  * @param networkTypeOpt string designating the type of network to be launched
  * @param runtimeParams all runtime determined application settings
  */
@main
final case class StartupOpts(
  @arg(name = "config", short = 'c', doc = "file path to a user defined config file")
  userConfigPathOpt: Option[String] = None,
  @arg(name = "network", short = 'n', doc = "specify preset network by name")
  networkTypeOpt:    Option[NetworkType] = None,
  runtimeParams:     RuntimeOpts = RuntimeOpts()
)

/* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */

/** Parameters that control the application behavior that are specified at runtime.
  * These settings could just as well be defined in the config file but may be
  * useful to define them as command line parameters instead.
  *
  * @param seed a string used to deterministically generate key files (only applicable for local and private networks)
  * @param forgeOnStartup a boolean controlling whether the node should attempt forging immediately on start
  * @param apiKeyHash hash of API key
  */
@main
final case class RuntimeOpts(
  @arg(name = "seed", short = 's', doc = "string to deterministically generate keys on private and local networks")
  seed:           Option[String] = None,
  @arg(name = "forge", short = 'f', doc = "enable forging as soon as the node starts")
  forgeOnStartup: Flag = Flag(),
  @arg(name = "authEnabled", short = 'a', doc = "Allow the node to receive API requests")
  authEnabled: Flag = Flag(),
  @arg(name = "apiKeyHash", doc = "hash of API key")
  apiKeyHash:     Option[String] = None
) {

  /** Helper method to replace settings values with parameters passed from command line arguments
    * @param appSettings application settings read from the configuration file
    * @return an updated appSettings instance
    */
  def overrideWithCmdArgs(appSettings: AppSettings): AppSettings = {
    val rpcApiSettings = appSettings.rpcApi.copy(
      authEnabled = appSettings.rpcApi.authEnabled || authEnabled.value,
      apiKeyHash = apiKeyHash.fold[String](appSettings.rpcApi.apiKeyHash)(a => a)
    )
    val privateTestnetSettings =
      appSettings.forging.privateTestnet
        .map(_.copy(
          genesisSeed = seed.orElse(appSettings.forging.privateTestnet.flatMap(_.genesisSeed))
        ))
    val forgingSettings =
      appSettings.forging.copy(
        forgeOnStartup = appSettings.forging.forgeOnStartup || forgeOnStartup.value,
        privateTestnet = privateTestnetSettings
      )

    appSettings.copy(
      rpcApi = rpcApiSettings,
      forging = forgingSettings
    )
  }
}

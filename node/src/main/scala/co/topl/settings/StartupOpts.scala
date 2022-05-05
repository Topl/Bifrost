package co.topl.settings

import co.topl.utils.NetworkType
import mainargs._
import monocle.syntax.all._

import java.net.InetSocketAddress

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
@main
final case class StartupOpts(
  @arg(name = "config", short = 'c', doc = "file path to a user defined config file")
  userConfigPathOpt: Option[String] = None,
  @arg(name = "debug", short = 'd', doc = "Turn on debugging information")
  verbose: Flag = Flag(),
  @arg(name = "network", short = 'n', doc = "specify preset network by name")
  networkTypeOpt: Option[NetworkType] = None,
  runtimeParams:  RuntimeOpts = RuntimeOpts()
)

/* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */

/**
 * Parameters that control the application behavior that are specified at runtime.
 * These settings could just as well be defined in the config file but may be
 * useful to define them as command line parameters instead.
 *
 * @param seed a string used to deterministically generate key files (only applicable for local and private networks)
 * @param forgeOnStartup a boolean controlling whether the node should attempt forging immediately on start
 * @param apiKeyHash hash of API key
 */
@main
final case class RuntimeOpts(
  @arg(name = "seed", short = 's', doc = "String used to deterministically generate addresses during startup")
  seed: Option[String] = None,
  @arg(name = "forge", short = 'f', doc = "Enable forging as soon as the node starts")
  forgeOnStartup: Flag = Flag(),
  @arg(name = "disableAuth", doc = "Allow the node to receive API requests (via JSON-RPC) without an API key")
  disableAuth: Flag = Flag(),
  @arg(
    name = "apiKeyHash",
    doc =
      "If API key protection is enabled, this argument specifies the Blake2b256 hash of API key required by the JSON-RPC server "
  )
  apiKeyHash: Option[String] = None,
  @arg(name = "knownPeers", short = 'k', doc = "List of IP addresses and ports of known peers, separated by commas")
  knownPeers: Option[String] = None,
  @arg(name = "networkBindAddress", short = 'a', doc = "Network address to bind to")
  networkBindAddress: Option[String] = None,
  @arg(name = "rpcBindAddress", short = 'r', doc = "Local network address to bind to")
  rpcBindAddress: Option[String] = None
) {

  /**
   * Helper method to replace settings values with parameters passed from command line arguments
   * NOTE: The default behavior is to defer to CLI arguments
   * @param appSettings application settings read from the configuration file
   * @return an updated appSettings instance
   */
  def overrideWithCmdArgs(appSettings: AppSettings): AppSettings =
    appSettings
      // seed
      .focus(_.forging.addressGenerationSettings)
      .modify { addrGenSettings =>
        if (seed.isDefined) {
          addrGenSettings
            .focus(_.addressSeedOpt)
            .modify(_ => seed)
            .focus(_.strategy)
            .modify(_ => AddressGenerationStrategies.FromSeed)
        } else addrGenSettings
      }

      // forge
      .focus(_.forging.forgeOnStartup)
      .replace(appSettings.forging.forgeOnStartup || forgeOnStartup.value)

      // disableAuth
      .focus(_.rpcApi.disableAuth)
      .replace(appSettings.rpcApi.disableAuth || disableAuth.value)

      // apiKeyHash
      .focus(_.rpcApi.apiKeyHash)
      .modify(configKey =>
        apiKeyHash match {
          case Some(cliKey) => cliKey
          case None         => configKey
        }
      )

      // knownPeers
      .focus(_.network.knownPeers)
      .modify(configPeers =>
        knownPeers match {
          case Some(peersString) =>
            peersString.split(",").map { peer =>
              val split = peer.split(":")
              new InetSocketAddress(split(0), split(1).toInt)
            }
          case None => configPeers
        }
      )

      // networkBindAddress
      .focus(_.network.bindAddress)
      .modify(configAddr =>
        networkBindAddress match {
          case Some(addrStr) =>
            val split = addrStr.split(":")
            new InetSocketAddress(split(0), split(1).toInt)
          case None => configAddr
        }
      )

      // rpcBindAddress
      .focus(_.rpcApi.bindAddress)
      .modify(configAddr =>
        rpcBindAddress match {
          case Some(addrStr) =>
            val split = addrStr.split(":")
            new InetSocketAddress(split(0), split(1).toInt)
          case None => configAddr
        }
      )
}

object StartupOptsImplicits {

  /**
   * networkReader, runtimeOptsParser, and startupOptsParser are defined here in a specific order
   *  to define the runtime command flags using mainargs. StartupOpts has to be last as it uses NetworkType
   *  and RuntimeOpts internally
   */
  implicit object networkReader
      extends TokensReader[NetworkType](
        shortName = "network",
        str =>
          NetworkType.pickNetworkType(str.head) match {
            case Some(net) => Right(net)
            case None      => Left("No valid network found with that name")
          }
      )

  implicit def runtimeOptsParser: ParserForClass[RuntimeOpts] = ParserForClass[RuntimeOpts]
  implicit def startupOptsParser: ParserForClass[StartupOpts] = ParserForClass[StartupOpts]
}

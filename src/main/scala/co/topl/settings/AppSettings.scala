package co.topl.settings

import java.io.File
import java.net.InetSocketAddress

import co.topl.http.api.NamespaceSelector
import co.topl.utils.{Logging, NetworkTimeProviderSettings}
import com.typesafe.config.{Config, ConfigFactory}
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

import scala.concurrent.duration._

case class ApplicationSettings(
  dataDir:     Option[String],
  keyFileDir:  Option[String],
  enablePBR:   Boolean,
  enableTBR:   Boolean,
  nodeKeys:    Option[Set[String]],
  version:     Version,
  cacheExpire: Int,
  cacheSize:   Int
)

case class RPCApiSettings(
  bindAddress:       InetSocketAddress,
  apiKeyHash:        String,
  timeout:           FiniteDuration,
  verboseAPI:        Boolean,
  namespaceSelector: NamespaceSelector
)

case class NetworkSettings(
  addedMaxDelay:           Option[FiniteDuration],
  agentName:               String,
  applicationNameLimit:    Int,
  bindAddress:             InetSocketAddress,
  connectionTimeout:       FiniteDuration,
  controllerTimeout:       Option[FiniteDuration],
  deadConnectionTimeout:   FiniteDuration,
  declaredAddress:         Option[InetSocketAddress],
  deliveryTimeout:         FiniteDuration,
  desiredInvObjects:       Int,
  getPeersInterval:        FiniteDuration,
  handshakeTimeout:        FiniteDuration,
  knownPeers:              Seq[InetSocketAddress],
  magicBytes:              Array[Byte],
  maxConnections:          Int,
  maxDeliveryChecks:       Int,
  maxHandshakeSize:        Int,
  maxInvObjects:           Int,
  maxModifiersCacheSize:   Int,
  maxChainCacheDepth:      Int,
  maxPacketSize:           Int,
  maxPeerSpecObjects:      Int,
  nodeName:                String,
  penaltySafeInterval:     FiniteDuration,
  penaltyScoreThreshold:   Int,
  syncInterval:            FiniteDuration,
  syncIntervalStable:      FiniteDuration,
  syncStatusRefresh:       FiniteDuration,
  syncStatusRefreshStable: FiniteDuration,
  syncTimeout:             Option[FiniteDuration],
  temporalBanDuration:     FiniteDuration,
  upnpDiscoverTimeout:     Option[FiniteDuration],
  upnpEnabled:             Boolean,
  upnpUseRandom:           Option[Boolean],
  upnpGatewayTimeout:      Option[FiniteDuration]
)

case class ForgingSettings(
  blockGenerationDelay: FiniteDuration,
  protocolVersions:     List[ProtocolSettings],
  forgeOnStartup:       Boolean,
  privateTestnet:       Option[PrivateTestnetSettings]
)

case class PrivateTestnetSettings(
  numTestnetAccts:   Int,
  testnetBalance:    Long,
  initialDifficulty: Long,
  genesisSeed:       Option[String]
)

case class AppSettings(
  application: ApplicationSettings,
  network:     NetworkSettings,
  forging:     ForgingSettings,
  rpcApi:      RPCApiSettings,
  ntp:         NetworkTimeProviderSettings
)

object AppSettings extends Logging with SettingsReaders {

  protected val configPath: String = "bifrost"

  /** Produces an application settings class, and modify the default settings if user options are provided
    *
    * @param startupOpts startup options such as the path of the user defined config and network type
    * @return application settings
    */
  def read(startupOpts: StartupOpts = StartupOpts.empty): AppSettings = {
    val settingFromConfig = fromConfig(readConfig(startupOpts))
    startupOpts.runtimeParams.overrideWithCmdArgs(settingFromConfig)
  }

  /** Produces an application settings class by reading the specified HOCON configuration file
    *
    * @param config config factory compatible configuration
    * @return application settings
    */
  def fromConfig(config: Config): AppSettings = config.as[AppSettings](configPath)

  /** Based on the startup arguments given by the user, modify and return the default application config
    *
    * @param args startup options such as the path of the user defined config and network type
    * @return config factory compatible configuration
    */
  def readConfig(args: StartupOpts): Config = {

    val networkPath = args.networkTypeOpt.flatMap { networkType =>
      // todo: JAA - check if this works with a fat-jar since resources are no longer in this location
      Option(s"src/main/resources/${networkType.verboseName}.conf")
    }

    val networkName: String = args.networkTypeOpt.flatMap(networkType => Option(networkType.verboseName)).getOrElse {
      log.warn(s"${Console.YELLOW}No network specified, running as local testnet.${Console.RESET}")
      "No Network Specified"
    }

    val networkConfigFileOpt = for {
      filePathOpt <- networkPath
      file = new File(filePathOpt)
      if file.exists
    } yield file

    val userConfigFileOpt = for {
      filePathOpt <- args.userConfigPathOpt
      file = new File(filePathOpt)
      if file.exists
    } yield file

    (userConfigFileOpt, networkConfigFileOpt) match {
      /** If there are user provided settings or network type, overwrite default settings with user specified ones */
      case (Some(file), None) ⇒
        log.warn(
          s"${Console.YELLOW}Found custom settings. " +
          s"Using default settings for ones not specified in custom Settings${Console.RESET}"
        )
        val config = ConfigFactory.parseFile(file)
        ConfigFactory
          .defaultOverrides()
          .withFallback(config)
          .withFallback(ConfigFactory.defaultApplication())
          .resolve()

      case (None, Some(networkConfigFile)) ⇒
        log.warn(s"${Console.YELLOW}Using $networkName settings${Console.RESET}")
        val networkConfig = ConfigFactory.parseFile(networkConfigFile)
        ConfigFactory
          .defaultOverrides()
          .withFallback(networkConfig)
          .withFallback(ConfigFactory.defaultApplication())
          .resolve()

      case (Some(file), Some(networkConfigFile)) =>
        log.warn(
          s"${Console.YELLOW}Found custom settings. " +
          s"Using $networkName settings for ones not specified in custom Settings${Console.RESET}"
        )
        val config = ConfigFactory.parseFile(file)
        val networkConfig = ConfigFactory.parseFile(networkConfigFile)
        ConfigFactory
          .defaultOverrides()
          .withFallback(config)
          .withFallback(networkConfig)
          .withFallback(ConfigFactory.defaultApplication())
          .resolve()

      /** Use default settings if no startup options is found */
      case _ ⇒
        log.warn(s"${Console.YELLOW}No custom setting specified, using default configuration${Console.RESET}")
        ConfigFactory.load()
    }
  }
}

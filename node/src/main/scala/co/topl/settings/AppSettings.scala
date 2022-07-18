package co.topl.settings

import co.topl.consensus.GenesisProvider
import co.topl.network.utils.NetworkTimeProviderSettings
import co.topl.utils.Logging
import com.typesafe.config.{Config, ConfigFactory}
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.ceedubs.ficus.readers.EnumerationReader._

import java.io.File
import java.net.InetSocketAddress
import scala.concurrent.duration._

case class ApplicationSettings(
  cacheExpire:      Int,
  cacheSize:        Int,
  dataDir:          Option[String],
  keyFileDir:       Option[String],
  mempoolTimeout:   FiniteDuration,
  nodeKeys:         Option[Set[String]],
  rebroadcastCount: Int,
  version:          Version,
  genesis:          GenesisSettings
)

case class RPCApiSettings(
  bindAddress:           InetSocketAddress,
  disableAuth:           Boolean,
  apiKeyHash:            String,
  timeout:               FiniteDuration,
  verboseAPI:            Boolean,
  namespaceSelector:     NamespaceSelector,
  blockRetrievalLimit:   Int,
  blockIdRetrievalLimit: Int,
  txRetrievalLimit:      Int
)

case class NetworkSettings(
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
  maxChainCacheDepth:      Int,
  maxConnections:          Int,
  maxDeliveryChecks:       Int,
  maxHandshakeSize:        Int,
  maxInvObjects:           Int,
  maxModifiersCacheSize:   Int,
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
  blockGenerationDelay:      FiniteDuration,
  minTransactionFee:         Long,
  protocolVersions:          List[ProtocolSettings],
  forgeOnStartup:            Boolean,
  rewardsAddress:            Option[String], // String here since we don't know netPrefix when settings are read
  addressGenerationSettings: AddressGenerationSettings
)

case class AddressGenerationSettings(
  numberOfAddresses: Int,
  strategy:          AddressGenerationStrategies.Value,
  addressSeedOpt:    Option[String]
)

object AddressGenerationStrategies extends Enumeration {
  val FromSeed: AddressGenerationStrategies.Value = Value("fromSeed")
  val Random: AddressGenerationStrategies.Value = Value("random")
  val None: AddressGenerationStrategies.Value = Value("none")
}

case class GenesisSettings(
  strategy:      GenesisStrategies.Value,
  generated:     Option[GenesisProvider.Strategies.Generation],
  fromBlockJson: Option[GenesisProvider.Strategies.FromBlockJson]
)

object GenesisStrategies extends Enumeration {
  val Generated: GenesisStrategies.Value = Value("generated")
  val FromBlockJson: GenesisStrategies.Value = Value("fromBlockJson")
}

case class GjallarhornSettings(
  enableWallet:   Boolean,
  clusterEnabled: Boolean,
  clusterHost:    Option[String],
  clusterPort:    Option[Int]
)

case class ChainReplicatorSettings(
  enableChainReplicator:   Boolean,
  checkMissingBlock:       Boolean,
  checkMissingStartHeight: Long,
  blockCheckSize:          Int,
  actorStashSize:          Int,
  mempoolCheckSize:        Int,
  uri:                     Option[String],
  database:                Option[String],
  blockCollection:         String,
  confirmedTxCollection:   String,
  unconfirmedTxCollection: String
)

case class AppSettings(
  application:     ApplicationSettings,
  network:         NetworkSettings,
  gjallarhorn:     GjallarhornSettings,
  forging:         ForgingSettings,
  rpcApi:          RPCApiSettings,
  ntp:             NetworkTimeProviderSettings,
  chainReplicator: ChainReplicatorSettings
)

object AppSettings extends Logging with SettingsReaders {

  protected val configPath: String = "bifrost"

  /**
   * Produces an application settings class, and modify the default settings if user options are provided
   *
   * @param startupOpts startup options such as the path of the user defined config and network type
   * @return application settings
   */
  def read(startupOpts: StartupOpts = StartupOpts()): (AppSettings, Config) = {
    val config: Config = readConfig(startupOpts)
    val settingFromConfig: AppSettings = fromConfig(config)
    val completeConfig: Config = clusterConfig(settingFromConfig, config)
    (startupOpts.runtimeParams.overrideWithCmdArgs(settingFromConfig), completeConfig)
  }

  /**
   * Produces an application settings class by reading the specified HOCON configuration file
   *
   * @param config config factory compatible configuration
   * @return application settings
   */
  private def fromConfig(config: Config): AppSettings = config.as[AppSettings](configPath)

  /**
   * Based on the startup arguments given by the user, modify and return the default application config
   *
   * @param args startup options such as the path of the user defined config and network type
   * @return config factory compatible configuration
   */
  private def readConfig(args: StartupOpts): Config = {

    val userConfig = args.userConfigPathOpt.fold(ConfigFactory.empty()) { uc =>
      val userFile = new File(uc)
      log.info(
        s"${Console.YELLOW}Attempting to load custom configuration from " +
        s"${userFile.getAbsolutePath}${Console.RESET}"
      )

      ConfigFactory.parseFile(userFile)
    }

    val networkConfig = args.networkTypeOpt match {
      case Some(value) =>
        log.info(s"${Console.YELLOW}Loading ${args.networkTypeOpt.get.verboseName} settings${Console.RESET}")
        ConfigFactory.load(this.getClass.getClassLoader, value.verboseName + ".conf")
      case None =>
        log.info(s"${Console.YELLOW}No network specified, running as private testnet.${Console.RESET}")
        ConfigFactory.empty()
    }

    // load config files from disk, if the above strings are empty then ConFigFactory will skip loading them
    ConfigFactory
      .defaultOverrides()
      .withFallback(userConfig)
      .withFallback(networkConfig)
      .withFallback(ConfigFactory.defaultApplication())
      .resolve()

  }

  private def clusterConfig(settings: AppSettings, config: Config): Config =
    if (settings.gjallarhorn.clusterEnabled) {
      ConfigFactory
        .parseString(s"""
      akka {
        actor.provider = cluster
        remote = {
          artery = {
            canonical.hostname = ${settings.gjallarhorn.clusterHost.getOrElse("0.0.0.0")}
            canonical.port = ${settings.gjallarhorn.clusterPort.getOrElse(0)}
          }
        }
      }
      """)
        .withFallback(config)
    } else {
      config
    }
}

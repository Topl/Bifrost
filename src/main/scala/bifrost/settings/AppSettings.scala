package bifrost.settings

import java.io.File
import java.net.InetSocketAddress

import bifrost.utils.{Logging, NetworkTimeProviderSettings}
import com.typesafe.config.{Config, ConfigFactory}
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

import scala.concurrent.duration._

case class RESTApiSettings(bindAddress: InetSocketAddress,
                           apiKeyHash: String,
                           corsAllowed: Boolean,
                           timeout: FiniteDuration)

case class NetworkSettings(addedMaxDelay: Option[FiniteDuration],
                           agentName: String,
                           appVersion: String,
                           bindAddress: InetSocketAddress,
                           connectionTimeout: FiniteDuration,
                           controllerTimeout: Option[FiniteDuration],
                           declaredAddress: Option[InetSocketAddress],
                           deliveryTimeout: FiniteDuration,
                           desiredInvObjects: Int,
                           getPeersInterval: FiniteDuration,
                           handshakeTimeout: FiniteDuration,
                           knownPeers: Seq[InetSocketAddress],
                           localOnly: Boolean,
                           magicBytes: Array[Byte],
                           maxConnections: Int,
                           maxDeliveryChecks: Int,
                           maxHandshakeSize: Int,
                           maxInvObjects: Int,
                           maxModifiersCacheSize: Int,
                           maxChainCacheDepth: Int,
                           maxPacketSize: Int,
                           maxPeerSpecObjects: Int,
                           nodeName: String,
                           penaltySafeInterval: FiniteDuration,
                           penaltyScoreThreshold: Int,
                           syncInterval: FiniteDuration,
                           syncIntervalStable: FiniteDuration,
                           syncStatusRefresh: FiniteDuration,
                           syncStatusRefreshStable: FiniteDuration,
                           syncTimeout: Option[FiniteDuration],
                           temporalBanDuration: FiniteDuration,
                           upnpDiscoverTimeout: Option[FiniteDuration],
                           upnpEnabled: Boolean,
                           upnpUseRandom: Option[Boolean],
                           upnpGatewayTimeout: Option[FiniteDuration]) {

}

case class ForgingSettings(MinimumDifficulty: Long,
                           InitialDifficulty: Long,
                           tryForging: Boolean,
                           targetBlockTime: FiniteDuration,
                           blockGenerationDelay: FiniteDuration,
                           version: Byte,
                           forkHeight: Long)

case class AppSettings(walletSeed: String,
                       keyFileDir: Option[String],
                       walletDir: Option[String],
                       dataDir: Option[String],
                       pbrDir: Option[String],
                       tbrDir: Option[String],
                       logDir: Option[String],
                       nodeKeys: Option[Set[String]],
                       rpcPort: Int,
                       verboseAPI: Boolean,
                       version: String,
                       cacheExpire: Int,
                       cacheSize: Int,
                       network: NetworkSettings,
                       forgingSettings: ForgingSettings,
                       restApi: RESTApiSettings,
                       ntp: NetworkTimeProviderSettings)

object AppSettings extends Logging with SettingsReaders {

  protected val configPath: String = "bifrost"

  def readConfig(args: StartupOpts): Config = {

    val networkPath = args.networkTypeOpt.flatMap{
      networkType =>
        // todo: JAA - check if this works with a fat-jar since resources are no longer in this location
        Option(s"src/main/resources/${networkType.verboseName}.conf")
    }

    args.networkTypeOpt.fold(log.warn("Running without network config"))(
      networkType => log.info(s"Running in ${networkType.verboseName} network mode"))

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
      /* If both are provided, user provided settings should override the default setting */
      case (Some(file), None) ⇒
        log.warn("Found custom settings. Using default settings for ones not specified in custom Settings")
        val config = ConfigFactory.parseFile(file)
        ConfigFactory
          .defaultOverrides()
          .withFallback(config)
          .withFallback(ConfigFactory.defaultApplication())
          .withFallback(ConfigFactory.defaultReference())
          .resolve()
      case (None, Some(networkConfigFile)) ⇒
//        log.warn("No custom settings provided. Starting with default settings!")
        val config = ConfigFactory.parseFile(networkConfigFile)
        ConfigFactory
        .defaultOverrides()
        .withFallback(config)
        .withFallback(ConfigFactory.defaultReference())
        .resolve()
      case (Some(file), Some(networkConfigFile)) =>
        log.warn(s"Found custom settings. Using network settings for ones not specified in custom Settings")
        val config = ConfigFactory.parseFile(file)
        val networkConfig = ConfigFactory.parseFile(networkConfigFile)
        ConfigFactory
          .defaultOverrides()
          .withFallback(config)
          .withFallback(networkConfig)
          .withFallback(ConfigFactory.defaultReference())
          .resolve()
      case _ ⇒
        log.warn("No custom setting specified, using default configuration")
        ConfigFactory.load()
    }
  }

  def read(startupOpts: StartupOpts = StartupOpts.empty): AppSettings = {
    fromConfig(readConfig(startupOpts))
  }

  def fromConfig(config: Config): AppSettings = {
    config.as[AppSettings](configPath)
  }
}
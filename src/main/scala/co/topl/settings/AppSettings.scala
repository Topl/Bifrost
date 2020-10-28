package co.topl.settings

import java.io.File
import java.net.InetSocketAddress

import co.topl.utils.{Logging, NetworkTimeProviderSettings}
import com.typesafe.config.{Config, ConfigFactory}
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

import scala.concurrent.duration._

case class ApplicationSettings (dataDir: Option[String],
                                keyFileDir: Option[String],
                                enablePBR: Boolean,
                                enableTBR: Boolean,
                                nodeKeys: Option[Set[String]],
                                version: Version,
                                cacheExpire: Int,
                                cacheSize: Int)

case class RESTApiSettings (bindAddress: InetSocketAddress,
                            apiKeyHash : String,
                            corsAllowed: Boolean,
                            timeout    : FiniteDuration,
                            verboseAPI : Boolean)

case class NetworkSettings (addedMaxDelay: Option[FiniteDuration],
                            agentName: String,
                            applicationNameLimit: Int,
                            bindAddress             : InetSocketAddress,
                            connectionTimeout       : FiniteDuration,
                            controllerTimeout       : Option[FiniteDuration],
                            declaredAddress         : Option[InetSocketAddress],
                            deliveryTimeout: FiniteDuration,
                            desiredInvObjects: Int,
                            getPeersInterval        : FiniteDuration,
                            handshakeTimeout        : FiniteDuration,
                            knownPeers              : Seq[InetSocketAddress],
                            magicBytes: Array[Byte],
                            maxConnections          : Int,
                            maxDeliveryChecks       : Int,
                            maxHandshakeSize        : Int,
                            maxInvObjects           : Int,
                            maxModifiersCacheSize   : Int,
                            maxChainCacheDepth      : Int,
                            maxPacketSize           : Int,
                            maxPeerSpecObjects: Int,
                            nodeName: String,
                            penaltySafeInterval: FiniteDuration,
                            penaltyScoreThreshold: Int,
                            syncInterval            : FiniteDuration,
                            syncIntervalStable      : FiniteDuration,
                            syncStatusRefresh       : FiniteDuration,
                            syncStatusRefreshStable : FiniteDuration,
                            syncTimeout             : Option[FiniteDuration],
                            temporalBanDuration     : FiniteDuration,
                            upnpDiscoverTimeout     : Option[FiniteDuration],
                            upnpEnabled             : Boolean,
                            upnpUseRandom           : Option[Boolean],
                            upnpGatewayTimeout      : Option[FiniteDuration])

case class ForgingSettings ( blockGenerationDelay: FiniteDuration,
                             protocolVersions    : List[ProtocolSettings],
                             privateTestnet      : Option[PrivateTestnetSettings])

case class PrivateTestnetSettings (numTestnetAccts  : Int,
                                   testnetBalance   : Long,
                                   initialDifficulty: Long)

case class AppSettings (application: ApplicationSettings,
                        network    : NetworkSettings,
                        forging    : ForgingSettings,
                        restApi    : RESTApiSettings,
                        ntp        : NetworkTimeProviderSettings)

object AppSettings extends Logging with SettingsReaders {

  protected val configPath: String = "bifrost"

  /**
    *
    * @param startupOpts
    * @return
    */
  def read (startupOpts: StartupOpts = StartupOpts.empty): AppSettings = {
    fromConfig(readConfig(startupOpts))
  }

  /**
    * Produces an application settings class by reading the specified HOCON configuration file
    * @param config config factory compatible configuration
    * @return
    */
  def fromConfig (config: Config): AppSettings = config.as[AppSettings](configPath)

  /**
    *
    * @param args
    * @return
    */
  def readConfig (args : StartupOpts): Config = {

    val networkPath = args.networkTypeOpt.flatMap {
      networkType =>
        // todo: JAA - check if this works with a fat-jar since resources are no longer in this location
        Option(s"src/main/resources/${networkType.verboseName}.conf")
    }

    args.networkTypeOpt.fold(log.warn("No network specified, running as local testnet."))(
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
          .resolve()

      case (None, Some(networkConfigFile)) ⇒
        val config = ConfigFactory.parseFile(networkConfigFile)
        ConfigFactory
          .defaultOverrides()
          .withFallback(config)
          .withFallback(ConfigFactory.defaultApplication())
          .resolve()

      case (Some(file), Some(networkConfigFile)) =>
        log.warn(s"Found custom settings. Using network settings for ones not specified in custom Settings")
        val config = ConfigFactory.parseFile(file)
        val networkConfig = ConfigFactory.parseFile(networkConfigFile)
        ConfigFactory
          .defaultOverrides()
          .withFallback(config)
          .withFallback(networkConfig)
          .withFallback(ConfigFactory.defaultApplication())
          .resolve()

      case _ ⇒
        log.warn("No custom setting specified, using default configuration")
        ConfigFactory.load()
    }
  }
}
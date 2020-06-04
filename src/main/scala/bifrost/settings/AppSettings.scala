package bifrost.settings

import java.io.File
import java.net.InetSocketAddress

import bifrost.utils.Logging
import com.typesafe.config.{Config, ConfigFactory}
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

import scala.concurrent.duration._

case class NetworkSettings(name: String,
                       bindAddress: InetSocketAddress,
                       myAddress: String,
                       upnp: Boolean,
                       upnpGatewayTimeout: Option[FiniteDuration],
                       upnpDiscoverTimeout: Option[FiniteDuration],
                       connectionTimeout: Int,
                       handshakeTimeout: Int,
                       addedMaxDelay: Option[Int],
                       maxConnections: Int,
                       networkChunkSize: Int,
                       localOnly: Boolean,
                       port: Int,
                       knownPeers: Seq[InetSocketAddress])

case class ForgingSettings(offlineGeneration: Boolean,
                           posAttachmentSize: Int,
                           targetBlockTime: Long,
                           version: Byte,
                           forkHeight: Long)

case class AppSettings(walletSeed: String,
                       keyFileDir: Option[String],
                       walletDir: Option[String],
                       dataDir: Option[String],
                       pbrDir: Option[String],
                       tbrDir: Option[String],
                       logDir: Option[String],
                       rpcPort: Int,
                       blockGenerationDelay: Long,
                       cors: Boolean,
                       verboseAPI: Boolean,
                       version: ApplicationVersion,
                       network: NetworkSettings,
                       forgingSettings: ForgingSettings)

object AppSettings extends Logging with SettingsReaders {

  protected val configPath: String = "bifrost"

  def readConfig(path: Option[String], configPath: String): Config = {

    val fileOpt: Option[File] = path.map(fileName ⇒ new File(fileName)).filter(_.exists())
      .orElse(path.flatMap(fileName ⇒ Option(getClass.getClassLoader.getResource(fileName)))
        .map(r ⇒ new File(r.toURI)).filter(_.exists()))

    val config = fileOpt match {
      case None ⇒
        log.warn("No configuration file was provided, using default configuration")
        ConfigFactory.load()
      case Some(file) ⇒
        val configFile = ConfigFactory.parseFile(file)
        if(!configFile.hasPath(configPath)) {
          throw new Error("Configuration file does not contain Bifrost settings object")
        }
        ConfigFactory
        .defaultOverrides()
        .withFallback(configFile)
        .withFallback(ConfigFactory.defaultApplication())
        .withFallback(ConfigFactory.defaultReference())
        .resolve()
    }

    config
  }

  def read(userConfigPath: Option[String]): AppSettings = {
    fromConfig(readConfig(userConfigPath, configPath))
  }

  def fromConfig(config: Config): AppSettings = {
    config.as[AppSettings](configPath)
  }
}
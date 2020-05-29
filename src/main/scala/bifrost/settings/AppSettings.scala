package bifrost.settings

import java.io.File
import java.net.InetSocketAddress

import com.typesafe.config.{Config, ConfigFactory}
import bifrost.utils.Logging

case class P2pSettings(name: String,
                       bindAddress: String,
                       myAddress: String,
                       upnp: Boolean,
                       upnpGatewayTimeout: Option[Boolean],
                       upnpDiscoverTimeout: Option[Boolean],
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
                       p2pSettings: P2pSettings,
                       forgingSettings: ForgingSettings)

object AppSettings extends Logging with SettingsReaders {

  def readConfig(path: Option[String]): Config = {

    val fileOpt: Option[File] = path.map(fileName ⇒ new File(fileName)).filter(_.exists())
      .orElse(path.flatMap(fileName ⇒ Option(getClass.getClassLoader.getResource(fileName)))
        .map(r ⇒ new File(r.toURI)).filter(_.exists()))

    val config = fileOpt match {
      case None ⇒
        log.warn("No configuration file was provided, using default configuration")
        ConfigFactory.load()
      case Some(file) ⇒
        val configFile = ConfigFactory.parseFile(file)
        if(!configFile.hasPath("bifrost")) {
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
}
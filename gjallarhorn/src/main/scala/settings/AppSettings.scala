package settings

import java.io.File
import java.net.InetSocketAddress

import com.typesafe.config.{Config, ConfigFactory}
import utils.Logging
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

case class AppSettings(declaredAddress: String,
                        rpcPort: Int,
                       requestPort: Int,
                       requestAddress: String,
                       keyFileDir: String,
                       chainProvider: String,
                       apiKeyHash: String)


object AppSettings extends Logging with SettingsReaders {

  protected val configPath: String = "gjallarhorn"

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
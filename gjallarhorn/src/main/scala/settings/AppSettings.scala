package settings

import java.io.File
import java.net.InetSocketAddress

import com.typesafe.config.{Config, ConfigFactory}
import pureconfig._
import pureconfig.generic.semiauto._
import http.NamespaceSelector
import utils.Logging
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import pureconfig.ConfigReader.Result

import scala.concurrent.duration.FiniteDuration

case class ApplicationSettings(declaredAddress: String,
                               var keyFileDir: String,
                               var chainProvider: ChainProvider,
                         /*      var communicationMode: String,
                               var bifrostApiKey: String,*/
                               var defaultChainProviders: List[ChainProvider])

object ApplicationSettings {
  implicit val applicationSettingsReader: ConfigReader[ApplicationSettings] = deriveReader[ApplicationSettings]
}

case class RPCApiSettings(bindHostname:      String,
                          bindPort:          Int,
                          apiKeyHash:        String,
                          timeout:           FiniteDuration,
                          verboseAPI:        Boolean,
                          namespaceSelector: NamespaceSelector)

object RPCApiSettings {
  implicit val rpcApiSettingsReader: ConfigReader[RPCApiSettings] = ConfigReader.forProduct6("bindHostname",
    "bindPort", "apiKeyHash", "timeout", "verboseAPI", "namespaceSelector")(
    RPCApiSettings(_,_,_,_,_,_))
}

case class AppSettings(application:  ApplicationSettings,
                       rpcApi:       RPCApiSettings)


object AppSettings extends Logging with SettingsReaders {

  protected val configPath: String = "gjallarhorn"
  implicit val appSettingsReader: ConfigReader[AppSettings] = deriveReader[AppSettings]

  def readFile(args: StartupOpts): Option[File] = {

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
        Some(file)
        /*val config = ConfigFactory.parseFile(file)
        ConfigFactory
          .defaultOverrides()
          .withFallback(config)
          .withFallback(ConfigFactory.defaultApplication())
          .withFallback(ConfigFactory.defaultReference())
          .resolve()*/
      case _ ⇒
        log.warn("No custom setting specified, using default configuration")
        None
      //ConfigFactory.load()
    }
  }

  def read(startupOpts: StartupOpts = StartupOpts.empty): AppSettings = {
    readFile(startupOpts) match {
      case Some (file) =>
        ConfigSource.file(file).at(configPath).load[AppSettings] match {
          case Right(config) => config
          case Left(_) =>
            log.info(s"Config: ${ConfigSource.default.toString}")
            ConfigSource.default.at(configPath).load[AppSettings] match {
              case Right(conf) => conf
              case Left(error) => throw new Exception (s"Error load config file: $error")
            }
        }
      case None =>
        ConfigSource.default.at(configPath).load[AppSettings] match {
          case Right(conf) => conf
          case Left(error) => throw new Exception (s"Error load config file: $error")
        }
    }
  }

  def readConfig(file: Option[File]): Config = {
    file match {
      case Some(startupFile) =>
        ConfigSource.file(startupFile).config() match {
          case Right(config) => config
          case Left(error) => throw new Exception(s"Could not generate config from given file: $file. $error")
        }
      case None => ConfigSource.default.config() match {
        case Right(config) => config
        case Left(error) => throw new Exception(s"Could not generate config from default configuration. $error")
      }
    }

  }

/*  def fromConfig(config: Config): AppSettings = {
    config.as[AppSettings](configPath)
  }*/
}
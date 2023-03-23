package co.topl.genusServer

import cats.Show
import com.typesafe.config.Config
import pureconfig.ConfigSource
import pureconfig.generic.auto._

case class ApplicationConfig(genus: ApplicationConfig.Genus)

object ApplicationConfig {

  case class Genus(
    rpcHost:           String,
    rpcPort:           Int,
    rpcNodeHost:       String,
    rpcNodePort:       Int,
    rpcNodeTls:        Boolean,
    orientDbDirectory: String,
    orientDbUser:      String,
    orientDbPassword:  String
  )

  def unsafe(config: Config): ApplicationConfig =
    ConfigSource.fromConfig(config).loadOrThrow[ApplicationConfig]

  implicit val showApplicationConfig: Show[ApplicationConfig] =
    Show.fromToString
}

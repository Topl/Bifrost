package co.topl.genusServer

import cats.Show
import com.typesafe.config.Config
import pureconfig.ConfigSource
import pureconfig.generic.auto._

case class ApplicationConfig(
  rpcHost: String,
  rpcPort: Int
)

object ApplicationConfig {

  def unsafe(config: Config): ApplicationConfig =
    ConfigSource.fromConfig(config).loadOrThrow[ApplicationConfig]

  implicit val showApplicationConfig: Show[ApplicationConfig] =
    Show.fromToString
}

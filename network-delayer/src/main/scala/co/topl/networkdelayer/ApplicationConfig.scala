package co.topl.networkdelayer

import cats.Show
import com.typesafe.config.Config
import pureconfig.ConfigSource
import pureconfig.generic.auto._

import scala.concurrent.duration.FiniteDuration

case class ApplicationConfig(
  routes: List[ApplicationConfig.Route]
)

object ApplicationConfig {

  case class Route(
    bindHost:        String,
    bindPort:        Int,
    destinationHost: String,
    destinationPort: Int,
    throttle:        Option[Route.Throttle]
  )

  object Route {
    case class Throttle(latency: FiniteDuration, downloadBytesPerSecond: Long, uploadBytesPerSecond: Long)
  }

  def unsafe(config: Config): ApplicationConfig =
    ConfigSource.fromConfig(config).loadOrThrow[ApplicationConfig]

  implicit val showApplicationConfig: Show[ApplicationConfig] =
    Show.fromToString
}

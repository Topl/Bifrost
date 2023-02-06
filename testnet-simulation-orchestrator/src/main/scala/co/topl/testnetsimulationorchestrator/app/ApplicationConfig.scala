package co.topl.testnetsimulationorchestrator.app

import cats.Show
import com.typesafe.config.Config
import monocle.macros.Lenses
import pureconfig.ConfigSource
import pureconfig.generic.auto._

@Lenses
case class ApplicationConfig(
  simulationOrchestrator: ApplicationConfig.SimulationOrchestrator
)

object ApplicationConfig {

  @Lenses
  case class SimulationOrchestrator(
    kubernetes: SimulationOrchestrator.Kubernetes,
    scenario:   SimulationOrchestrator.Scenario,
    nodes:      List[SimulationOrchestrator.Node],
    publish:    SimulationOrchestrator.Publish
  )

  object SimulationOrchestrator {

    @Lenses
    case class Kubernetes(namespace: String)

    @Lenses
    case class Scenario(targetHeight: Long, transactionsPerSecond: Double)

    @Lenses
    case class Node(name: String, host: String, port: Int)

    @Lenses
    case class Publish(bucket: String, filePrefix: String)
  }

  def unsafe(args: Args, config: Config): ApplicationConfig =
    ConfigSource.fromConfig(config).loadOrThrow[ApplicationConfig]

  implicit val showApplicationConfig: Show[ApplicationConfig] =
    Show.fromToString
}

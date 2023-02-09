package co.topl.transactiongenerator.app

import cats.Show
import com.typesafe.config.Config
import monocle.macros.Lenses
import pureconfig.ConfigSource
import pureconfig.generic.auto._

import scala.concurrent.duration.FiniteDuration

@Lenses
case class ApplicationConfig(
  transactionGenerator: ApplicationConfig.TransactionGenerator
)

object ApplicationConfig {

  @Lenses
  case class TransactionGenerator(
    rpc:         TransactionGenerator.Rpc,
    generator:   TransactionGenerator.Generator,
    broadcaster: TransactionGenerator.Broadcaster,
    mempool:     TransactionGenerator.Mempool,
    parallelism: TransactionGenerator.Parallelism
  )

  object TransactionGenerator {

    @Lenses
    case class Rpc(clients: Seq[String])

    @Lenses
    case class Generator(dataLength: Int, maxWalletSize: Int)

    @Lenses
    case class Broadcaster(tps: Double)

    @Lenses
    case class Mempool(period: FiniteDuration)

    @Lenses
    case class Parallelism(fetchHeader: Int, fetchBody: Int, fetchTransaction: Int, generateTx: Int)
  }

  def unsafe(args: Args, config: Config): ApplicationConfig =
    ConfigSource.fromConfig(config).loadOrThrow[ApplicationConfig]

  implicit val showApplicationConfig: Show[ApplicationConfig] =
    Show.fromToString
}

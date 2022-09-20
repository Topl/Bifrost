package co.topl.node

import cats.Show
import cats.data.NonEmptyChain
import cats.implicits._
import cats.kernel.Monoid
import co.topl.models.Slot
import co.topl.models.utility.Ratio
import co.topl.networking.p2p.DisconnectedPeer
import com.typesafe.config.{Config, ConfigFactory}
import pureconfig._
import pureconfig.generic.ProductHint
import pureconfig.generic.auto._
import pureconfig.configurable._

import java.net.InetSocketAddress
import scala.concurrent.duration.FiniteDuration
import scala.util.Try

case class ApplicationConfig(bifrost: ApplicationConfig.Bifrost)

object ApplicationConfig {

  case class Bifrost(
    data:      Bifrost.Data,
    staking:   Bifrost.Staking,
    p2p:       Bifrost.P2P,
    rpc:       Bifrost.RPC,
    mempool:   Bifrost.Mempool,
    bigBang:   Bifrost.BigBang,
    protocols: Map[Slot, Bifrost.Protocol]
  )

  object Bifrost {
    case class Data(directory: String)
    case class Staking(directory: String)

    case class P2P(
      bindHost:   String,
      bindPort:   Int,
      publicHost: String,
      publicPort: Int,
      knownPeers: List[DisconnectedPeer]
    )
    case class RPC(bindHost: String, bindPort: Int)
    case class Mempool(defaultExpirationSlots: Long, duplicateSpenderExpirationSlots: Long)
    sealed abstract class BigBang

    object BigBangs {

      case class Private(
        timestamp:        Long = System.currentTimeMillis() + 5_000L,
        stakerCount:      Int,
        localStakerIndex: Option[Int]
      ) extends BigBang
    }

    case class Protocol(
      fEffective:                 Ratio,
      vrfLddCutoff:               Int,
      vrfPrecision:               Int,
      vrfBaselineDifficulty:      Ratio,
      vrfAmplitude:               Ratio,
      chainSelectionKLookback:    Long,
      slotDuration:               FiniteDuration,
      operationalPeriodsPerEpoch: Long,
      kesKeyHours:                Int,
      kesKeyMinutes:              Int
    ) {
      import co.topl.typeclasses.implicits._

      val chainSelectionSWindow: Long =
        (Ratio(chainSelectionKLookback, 4L) * fEffective.inverse).round.toLong

      val epochLength: Long =
        chainSelectionKLookback * 6

      val operationalPeriodLength: Long =
        epochLength / operationalPeriodsPerEpoch
    }
  }

  implicit private val monoidConfig: Monoid[Config] =
    Monoid.instance(ConfigFactory.empty(), _ withFallback _)

  def createTypesafeConfig(cmdArgs: Args): Config =
    NonEmptyChain(
      ConfigSource.fromConfig(ConfigFactory.systemEnvironmentOverrides()),
      ConfigSource.resources("environment.conf"),
      ConfigSource.fromConfig(YamlConfig.loadResource("custom-config.yaml")),
      ConfigSource.default
    ).foldMapM(_.config()) match {
      case Right(value) => value.resolve()
      case Left(e)      => throw new IllegalStateException(e.toString)
    }

  def unsafe(cmdArgs: Args, config: Config): ApplicationConfig =
    ConfigSource.fromConfig(config).loadOrThrow[ApplicationConfig]

  implicit val ratioConfigReader: ConfigReader[Ratio] =
    ConfigReader.fromNonEmptyStringTry { str =>
      Try {
        val Array(numeratorStr, denominatorStr) = str.split('/')
        Ratio(BigInt(numeratorStr), BigInt(denominatorStr))
      }
    }

  private val defaultConfigFieldMapping = ConfigFieldMapping(CamelCase, KebabCase)

  implicit val knownPeersReader: ConfigReader[List[DisconnectedPeer]] =
    ConfigReader[String].emap(str =>
      Try(
        str.split(',').toList.filterNot(_.isEmpty).map { addr =>
          val Array(host, portStr) = addr.split(':')
          DisconnectedPeer(InetSocketAddress.createUnresolved(host, portStr.toInt), (0, 0))
        }
      ).toEither.leftMap(e => error.CannotConvert(str, "InetAddressList", e.getMessage))
    )

  implicit def slotMapReader[T: ConfigReader]: ConfigReader[Map[Slot, T]] =
    genericMapReader[Slot, T](v => v.toLongOption.toRight(error.CannotConvert(v, "Slot", "Not a long")))

  implicit val bifrostProductHint: ProductHint[Bifrost] =
    ProductHint[Bifrost](ConfigFieldMapping {
      case "p2p" => "p2p"
      case v     => defaultConfigFieldMapping(v)
    })

  implicit val showApplicationConfig: Show[ApplicationConfig] =
    Show.fromToString
}

package co.topl.node

import cats.Show
import co.topl.models.utility.Ratio
import co.topl.networking.p2p.DisconnectedPeer
import com.typesafe.config.Config
import pureconfig._
import pureconfig.generic.ProductHint
import pureconfig.generic.auto._

import java.net.InetSocketAddress
import scala.concurrent.duration.FiniteDuration
import scala.util.Try

case class ApplicationConfig(bifrost: ApplicationConfig.Bifrost)

object ApplicationConfig {

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
    ConfigReader[String].map(str =>
      str.split(',').toList.filterNot(_.isEmpty).map { addr =>
        val Array(host, portStr) = addr.split(':')
        DisconnectedPeer(InetSocketAddress.createUnresolved(host, portStr.toInt), (0, 0))
      }
    )

  implicit val bifrostProductHint: ProductHint[Bifrost] =
    ProductHint[Bifrost](ConfigFieldMapping {
      case "p2p" => "p2p"
      case v     => defaultConfigFieldMapping(v)
    })

  implicit val showApplicationConfig: Show[ApplicationConfig] =
    Show.fromToString

  case class Bifrost(
    data:      Bifrost.Data,
    staking:   Bifrost.Staking,
    p2p:       Bifrost.P2P,
    rpc:       Bifrost.RPC,
    mempool:   Bifrost.Mempool,
    bigBang:   Bifrost.BigBang,
    protocols: Map[String, Bifrost.Protocol]
  )

  object Bifrost {
    case class Data(directory: String)
    case class Staking(directory: String)

    /**
     * Settings for peer-to-peer networking
     * @param bindingHost The host on the local machine to bind to
     * @param bindingPort The port on the local machine to bind to
     * @param publicHost The host to tell other peers if they want to reach you publicly
     * @param publicPort The port to tell other peers if they want to reach you publicly
     * @param knownPeers A comma-delimited list of addresses to connect to initially.
     *                   Should be comma-delimited host:port
     *                   i.e. 1.2.3.4:9085,5.6.7.8:9085
     */
    case class P2P(
      bindingHost: String,
      bindingPort: Int,
      publicHost:  String,
      publicPort:  Int,
      knownPeers:  List[DisconnectedPeer]
    )
    case class RPC(bindingHost: String, bindingPort: Int)
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
}

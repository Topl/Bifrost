package co.topl.node

import cats.Show
import cats.implicits._
import co.topl.models.Slot
import co.topl.models.utility.Ratio
import co.topl.networking.p2p.{DisconnectedPeer, RemoteAddress}
import co.topl.numerics.implicits._
import com.typesafe.config.Config
import monocle._
import monocle.macros._
import pureconfig._
import pureconfig.generic.ProductHint
import pureconfig.generic.auto._
import pureconfig.configurable._

import scala.concurrent.duration.FiniteDuration
import scala.util.Try

// $COVERAGE-OFF$
@Lenses
case class ApplicationConfig(bifrost: ApplicationConfig.Bifrost, kamon: ApplicationConfig.Kamon)

object ApplicationConfig {

  @Lenses
  case class Bifrost(
    data:      Bifrost.Data,
    staking:   Bifrost.Staking,
    p2p:       Bifrost.P2P,
    rpc:       Bifrost.RPC,
    mempool:   Bifrost.Mempool,
    bigBang:   Bifrost.BigBang,
    protocols: Map[Slot, Bifrost.Protocol],
    cache:     Bifrost.Cache
  )

  object Bifrost {

    @Lenses
    case class Data(directory: String)

    @Lenses
    case class Staking(directory: String)

    @Lenses
    case class P2P(
      bindHost:     String,
      bindPort:     Int,
      publicHost:   String,
      publicPort:   Int,
      knownPeers:   List[DisconnectedPeer],
      experimental: Option[Boolean]
    )

    @Lenses
    case class RPC(bindHost: String, bindPort: Int)

    @Lenses
    case class Mempool(defaultExpirationSlots: Long, duplicateSpenderExpirationSlots: Long)
    sealed abstract class BigBang

    object BigBangs {

      @Lenses
      case class Private(
        timestamp:        Long = System.currentTimeMillis() + 5_000L,
        stakerCount:      Int,
        relativeStakes:   Option[List[Ratio]],
        localStakerIndex: Option[Int]
      ) extends BigBang
    }

    @Lenses
    case class Protocol(
      fEffective:                 Ratio,
      vrfLddCutoff:               Int,
      vrfPrecision:               Int,
      vrfBaselineDifficulty:      Ratio,
      vrfAmplitude:               Ratio,
      chainSelectionKLookback:    Long,
      slotDuration:               FiniteDuration,
      forwardBiasedSlotWindow:    Slot,
      operationalPeriodsPerEpoch: Long,
      kesKeyHours:                Int,
      kesKeyMinutes:              Int
    ) {

      val chainSelectionSWindow: Long =
        (Ratio(chainSelectionKLookback, 4L) * fEffective.inverse).round.toLong

      val epochLength: Long =
        chainSelectionKLookback * 6

      val operationalPeriodLength: Long =
        epochLength / operationalPeriodsPerEpoch

      val vrfCacheSize: Long =
        operationalPeriodLength * 4
    }

    @Lenses
    case class Cache(
      parentChildTree: Cache.CacheConfig,
      slotData:        Cache.CacheConfig,
      headers:         Cache.CacheConfig,
      bodies:          Cache.CacheConfig,
      transactions:    Cache.CacheConfig,
      spendableBoxIds: Cache.CacheConfig,
      epochBoundaries: Cache.CacheConfig,
      operatorStakes:  Cache.CacheConfig,
      registrations:   Cache.CacheConfig,
      blockHeightTree: Cache.CacheConfig
    )

    object Cache {

      @Lenses
      case class CacheConfig(maximumEntries: Long, ttl: Option[FiniteDuration])
    }
  }

  /**
   * Construct an ApplicationConfig based on the given command-line arguments and a merged HOCON config.
   *
   * May throw exceptions.
   */
  def unsafe(cmdArgs: Args, config: Config): ApplicationConfig = {
    val base = ConfigSource.fromConfig(config).loadOrThrow[ApplicationConfig]
    val genLens = GenLens[ApplicationConfig]
    def createF[B](lens: Lens[ApplicationConfig, B])(value: B): ApplicationConfig => ApplicationConfig =
      (appConf: ApplicationConfig) => lens.replace(value)(appConf)
    val simpleArgApplications =
      List[Option[ApplicationConfig => ApplicationConfig]](
        cmdArgs.runtime.dataDir.map(createF(genLens(_.bifrost.data.directory))),
        cmdArgs.runtime.stakingDir.map(createF(genLens(_.bifrost.staking.directory))),
        cmdArgs.runtime.rpcBindHost.map(createF(genLens(_.bifrost.rpc.bindHost))),
        cmdArgs.runtime.rpcBindPort.map(createF(genLens(_.bifrost.rpc.bindPort))),
        cmdArgs.runtime.p2pBindHost.map(createF(genLens(_.bifrost.p2p.bindHost))),
        cmdArgs.runtime.p2pBindPort.map(createF(genLens(_.bifrost.p2p.bindPort))),
        cmdArgs.runtime.knownPeers.map(parseKnownPeers).map(createF(genLens(_.bifrost.p2p.knownPeers)))
      ).flatten
        .foldLeft(base) { case (appConf, f) => f(appConf) }
    if (
      cmdArgs.runtime.testnetArgs.testnetTimestamp.nonEmpty ||
      cmdArgs.runtime.testnetArgs.testnetStakerCount.nonEmpty ||
      cmdArgs.runtime.testnetArgs.testnetStakerIndex.nonEmpty
    ) {
      val bigBangConfig =
        simpleArgApplications.bifrost.bigBang match {
          case p: Bifrost.BigBangs.Private =>
            p.copy(
              timestamp = cmdArgs.runtime.testnetArgs.testnetTimestamp.getOrElse(p.timestamp),
              stakerCount = cmdArgs.runtime.testnetArgs.testnetStakerCount.getOrElse(p.stakerCount),
              localStakerIndex = cmdArgs.runtime.testnetArgs.testnetStakerIndex.orElse(p.localStakerIndex)
            )
        }
      genLens(_.bifrost.bigBang).replace(bigBangConfig)(simpleArgApplications)
    } else {
      simpleArgApplications
    }
  }

  implicit val ratioConfigReader: ConfigReader[Ratio] =
    ConfigReader.fromNonEmptyStringTry { str =>
      Try {
        val Array(numeratorStr, denominatorStr) = str.split('/')
        Ratio(BigInt(numeratorStr), BigInt(denominatorStr))
      }
    }

  private val defaultConfigFieldMapping = ConfigFieldMapping(CamelCase, KebabCase)

  /**
   * Parses the given comma-delimited string of host:port combinations
   * i.e. "1.2.3.4:9095,5.6.7.8:9095"
   */
  private def parseKnownPeers(str: String): List[DisconnectedPeer] =
    str.split(',').toList.filterNot(_.isEmpty).map { addr =>
      val Array(host, portStr) = addr.split(':')
      DisconnectedPeer(RemoteAddress(host, portStr.toInt), (0, 0))
    }

  implicit val knownPeersReader: ConfigReader[List[DisconnectedPeer]] =
    ConfigReader[String].emap(str =>
      Try(
        parseKnownPeers(str)
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

  case class Kamon(enable: Boolean)
}
// $COVERAGE-ON$

package co.topl.node

import cats.Show
import cats.data.{Chain, NonEmptyChain}
import cats.implicits._
import cats.kernel.Monoid
import co.topl.models.Slot
import co.topl.models.utility.Ratio
import co.topl.networking.p2p.DisconnectedPeer
import com.typesafe.config.{Config, ConfigFactory}
import monocle._
import monocle.macros._
import monocle.macros.syntax.lens._
import pureconfig._
import pureconfig.generic.ProductHint
import pureconfig.generic.auto._
import pureconfig.configurable._

import java.net.InetSocketAddress
import java.nio.file.Paths
import scala.concurrent.duration.FiniteDuration
import scala.util.Try

@Lenses
case class ApplicationConfig(bifrost: ApplicationConfig.Bifrost)

object ApplicationConfig {

  @Lenses
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

    @Lenses
    case class Data(directory: String)

    @Lenses
    case class Staking(directory: String)

    @Lenses
    case class P2P(
      bindHost:   String,
      bindPort:   Int,
      publicHost: String,
      publicPort: Int,
      knownPeers: List[DisconnectedPeer]
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
    (
      argsToDebugConfigs(cmdArgs) ++
        Chain(ConfigSource.resources("environment.conf")) ++
        argsToUserConfigs(cmdArgs) ++
        Chain(
          ConfigSource.fromConfig(YamlConfig.loadResource("custom-config.yaml")),
          ConfigSource.default
        )
    ).foldMapM(_.config()) match {
      case Right(value) => value.resolve()
      case Left(e)      => throw new IllegalStateException(e.toString)
    }

  /**
   * Allow the --debug argument to enable the `CONFIG_FORCE_` environment variable syntax from Typesafe Config
   */
  private def argsToDebugConfigs(cmdArgs: Args): Chain[ConfigObjectSource] =
    Chain.fromOption(
      Option.when(cmdArgs.startup.debug.value)(
        ConfigSource.fromConfig(ConfigFactory.systemEnvironmentOverrides())
      )
    )

  private def argsToUserConfigs(cmdArgs: Args): Chain[ConfigObjectSource] =
    Chain
      .fromSeq(cmdArgs.startup.config)
      .map(name =>
        if (name.startsWith("resource://")) {
          val path = Paths.get(name)
          if (name.endsWith(".yaml") || name.endsWith(".yml"))
            ConfigSource.fromConfig(YamlConfig.load(path))
          else
            ConfigSource.file(path)
        } else {
          if (name.endsWith(".yaml") || name.endsWith(".yml"))
            ConfigSource.fromConfig(YamlConfig.loadResource(name))
          else
            ConfigSource.resources(name)
        }
      )

  def unsafe(cmdArgs: Args, config: Config): ApplicationConfig = {
    val base = ConfigSource.fromConfig(config).loadOrThrow[ApplicationConfig]
    val genLens = GenLens[ApplicationConfig]
    def createF[B](lens: Lens[ApplicationConfig, B])(value: B): ApplicationConfig => ApplicationConfig =
      (appConf: ApplicationConfig) => lens.replace(value)(appConf)
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
  }

  implicit val ratioConfigReader: ConfigReader[Ratio] =
    ConfigReader.fromNonEmptyStringTry { str =>
      Try {
        val Array(numeratorStr, denominatorStr) = str.split('/')
        Ratio(BigInt(numeratorStr), BigInt(denominatorStr))
      }
    }

  private val defaultConfigFieldMapping = ConfigFieldMapping(CamelCase, KebabCase)

  private def parseKnownPeers(str: String): List[DisconnectedPeer] =
    str.split(',').toList.filterNot(_.isEmpty).map { addr =>
      val Array(host, portStr) = addr.split(':')
      DisconnectedPeer(InetSocketAddress.createUnresolved(host, portStr.toInt), (0, 0))
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
}

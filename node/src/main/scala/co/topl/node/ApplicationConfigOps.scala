package co.topl.node

import cats.Show
import cats.implicits._
import co.topl.brambl.codecs.AddressCodecs.decodeAddress
import co.topl.brambl.models.LockAddress
import co.topl.config.ApplicationConfig
import co.topl.config.ApplicationConfig.Bifrost
import co.topl.config.ApplicationConfig.Bifrost.KnownPeer
import co.topl.models.Slot
import co.topl.models.utility.Ratio
import com.typesafe.config.Config
import monocle._
import monocle.macros._
import pureconfig._
import pureconfig.configurable._
import pureconfig.generic.ProductHint
import pureconfig.generic.auto._

import scala.util.Try

// $COVERAGE-OFF$

object ApplicationConfigOps {

  /**
   * Construct an ApplicationConfig based on the given command-line arguments and a merged HOCON config.
   *
   * May throw exceptions.
   */
  def unsafe(cmdArgs: Args, config: Config): ApplicationConfig = {
    val base = ConfigSource.fromConfig(config).loadOrThrow[ApplicationConfig]

    def createF[B](lens: Lens[ApplicationConfig, B])(value: B): ApplicationConfig => ApplicationConfig =
      (appConf: ApplicationConfig) => lens.replace(value)(appConf)

    val simpleArgApplications =
      List[Option[ApplicationConfig => ApplicationConfig]](
        cmdArgs.runtime.dataDir.map(createF(GenLens[ApplicationConfig](_.bifrost.data.directory))),
        cmdArgs.runtime.stakingDir.map(createF(GenLens[ApplicationConfig](_.bifrost.staking.directory))),
        cmdArgs.runtime.rpcBindHost.map(createF(GenLens[ApplicationConfig](_.bifrost.rpc.bindHost))),
        cmdArgs.runtime.rpcBindPort.map(createF(GenLens[ApplicationConfig](_.bifrost.rpc.bindPort))),
        cmdArgs.runtime.p2pBindHost.map(createF(GenLens[ApplicationConfig](_.bifrost.p2p.bindHost))),
        cmdArgs.runtime.p2pBindPort.map(createF(GenLens[ApplicationConfig](_.bifrost.p2p.bindPort))),
        cmdArgs.runtime.knownPeers
          .map(parseKnownPeers)
          .map(createF(GenLens[ApplicationConfig](_.bifrost.p2p.knownPeers))),
        cmdArgs.runtime.genusArgs.orientDbDir.map(createF(GenLens[ApplicationConfig](_.genus.orientDbDirectory))),
        cmdArgs.runtime.genusArgs.orientDbPassword.map(createF(GenLens[ApplicationConfig](_.genus.orientDbPassword)))
      ).flatten
        .foldLeft(
          if (cmdArgs.runtime.genusArgs.disableGenus.value)
            createF(GenLens[ApplicationConfig](_.genus.enable))(false)(base)
          else
            base
        ) { case (appConf, f) => f(appConf) }
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
      GenLens[ApplicationConfig](_.bifrost.bigBang).replace(bigBangConfig)(simpleArgApplications)
    } else {
      simpleArgApplications
    }
  }

  implicit val bigIntConfigReader: ConfigReader[BigInt] =
    ConfigReader.fromNonEmptyStringTry(str => Try(BigInt(str)))

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
  private def parseKnownPeers(str: String): List[KnownPeer] =
    str.split(',').toList.filterNot(_.isEmpty).map { addr =>
      val Array(host, portStr) = addr.split(':')
      KnownPeer(host, portStr.toInt)
    }

  implicit val knownPeersReader: ConfigReader[List[KnownPeer]] =
    ConfigReader[String].emap(str =>
      Try(
        parseKnownPeers(str)
      ).toEither.leftMap(e => error.CannotConvert(str, "InetAddressList", e.getMessage))
    )

  implicit val lockAddressReader: ConfigReader[LockAddress] =
    ConfigReader[String].emap(str =>
      decodeAddress(str).leftMap(e => error.CannotConvert(str, "LockAddress", e.toString))
    )

  implicit def slotMapReader[T: ConfigReader]: ConfigReader[Map[Slot, T]] =
    genericMapReader[Slot, T](v => v.toLongOption.toRight(error.CannotConvert(v, "Slot", "Not a long")))

  implicit val bifrostProductHint: ProductHint[Bifrost] =
    ProductHint[Bifrost](ConfigFieldMapping {
      case "p2p" => "p2p"
      case v     => defaultConfigFieldMapping(v)
    })

  implicit val showApplicationConfig: Show[ApplicationConfig] = {
    val base = Show.fromToString[ApplicationConfig]
    val sanitizer = GenLens[ApplicationConfig](_.genus.orientDbPassword).replace("SANITIZED")
    conf => base.show(sanitizer(conf))
  }
}
// $COVERAGE-ON$

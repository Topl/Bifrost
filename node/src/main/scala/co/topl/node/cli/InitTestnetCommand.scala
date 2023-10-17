package co.topl.node.cli

import cats.{MonadThrow, Show}
import cats.data.{EitherT, OptionT}
import cats.effect.std.{Console, SecureRandom}
import cats.effect.{Async, Sync}
import cats.implicits._
import co.topl.blockchain.{BigBang, PrivateTestnet, StakerInitializers, StakingInit}
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.box.Value.UpdateProposal
import co.topl.brambl.models.transaction.{IoTransaction, Schedule, UnspentTransactionOutput}
import co.topl.brambl.models.{Datum, Event}
import co.topl.brambl.syntax._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.config.ApplicationConfig
import co.topl.consensus.models.BlockId
import co.topl.crypto.hash.Blake2b256
import co.topl.node.ProtocolVersioner
import co.topl.node.cli.InitTestnetCommand.{DefaultProtocol, DefaultUpdateProposal}
import co.topl.node.models.{BlockBody, FullBlock, Ratio}
import co.topl.typeclasses.implicits._
import com.google.protobuf.ByteString
import com.google.protobuf.duration.Duration
import fs2.io.file.{Files, Path}
import quivr.models.{Int128, SmallData}
import co.topl.numerics.implicits._

import java.nio.charset.StandardCharsets
import scala.concurrent.duration._

object InitTestnetCommand {

  def apply[F[_]: Async: Console](appConfig: ApplicationConfig): StageResultT[F, Unit] =
    new InitTestnetCommandImpl[F](appConfig).command

  private[cli] val DefaultProtocol =
    ApplicationConfig.Bifrost.Protocol(
      minAppVersion = "2.0.0",
      fEffective = Ratio(15, 100),
      vrfLddCutoff = 50,
      vrfPrecision = 40,
      vrfBaselineDifficulty = Ratio(1, 20),
      vrfAmplitude = Ratio(1, 2),
      // 10x private testnet default, resulting in ~50 minute epochs
      chainSelectionKLookback = 500,
      slotDuration = 1.seconds,
      forwardBiasedSlotWindow = 50,
      operationalPeriodsPerEpoch = 24,
      kesKeyHours = 9,
      kesKeyMinutes = 9
    )

  private[cli] val DefaultUpdateProposal = BigBang.protocolToUpdateProposal(DefaultProtocol)

}

class InitTestnetCommandImpl[F[_]: Async](appConfig: ApplicationConfig)(implicit c: Console[F]) {

  private val intro: StageResultT[F, Unit] =
    writeMessage[F](
      "This utility generates a new blockchain.  The output is several files including a genesis block with the initial LVL and TOPL distribution and a staking directory for each genesis staker."
    )

  private val readSeed =
    writeMessage[F]("Enter a seed for this staker.  Leave blank to use a random seed.") >>
    readInput[F].semiflatMap {
      case "" =>
        SecureRandom.javaSecuritySecureRandom.flatMap(_.nextBytes(32))
      case lockAddressStr =>
        Sync[F].delay(new Blake2b256().hash(lockAddressStr.getBytes(StandardCharsets.UTF_8)))
    }

  /**
   * Read a lock address (or retry until a valid value is provided)
   */
  private val readLockAddress =
    (writeMessage[F]("Enter a LockAddress.  Leave blank to use HeightLock=1.") >>
      readInput[F].semiflatMap {
        case "" =>
          PrivateTestnet.HeightLockOneSpendingAddress.some.pure[F]
        case lockAddressStr =>
          EitherT
            .fromEither[F](co.topl.brambl.codecs.AddressCodecs.decodeAddress(lockAddressStr))
            .leftSemiflatTap(error => c.println(s"Invalid Lock Address. reason=$error input=$lockAddressStr"))
            .toOption
            .value
      }).untilDefinedM

  /**
   * Read an Int128 quantity (or retry until a valid value is provided)
   */
  private val readQuantity =
    readLowercasedInput.semiflatMap {
      case "" => PrivateTestnet.DefaultTotalStake.some.pure[F]
      case str =>
        EitherT(MonadThrow[F].catchNonFatal(BigInt(str)).attempt)
          .leftMap(_ => s"Invalid Quantity. input=$str")
          .ensure(s"Quantity too large. input=$str")(_.bitLength <= 128)
          .ensure(s"Quantity must be positive. input=$str")(_ > 0)
          .map(quantity => Int128(ByteString.copyFrom(quantity.toByteArray)))
          .leftSemiflatTap(c.println(_))
          .toOption
          .value
    }

  private val readStakerQuantity =
    (
      writeMessage[F]("How many TOPLs should this staker possess? default=10,000,000") >>
        readQuantity
    ).untilDefinedM

  private val readStaker: StageResultT[F, StakerInitializerWithQuantity] =
    for {
      seed        <- readSeed
      lockAddress <- readLockAddress
      quantity    <- readStakerQuantity
      initializer = StakerInitializers.Operator(
        seed,
        (appConfig.bifrost.protocols(0).kesKeyHours, appConfig.bifrost.protocols(0).kesKeyMinutes),
        lockAddress
      )
    } yield StakerInitializerWithQuantity(initializer, quantity, initializer.registrationTransaction(quantity))

  private val readStakers: StageResultT[F, List[StakerInitializerWithQuantity]] =
    for {
      _       <- writeMessage[F]("Please initialize the first staker.")
      staker0 <- readStaker
      otherStakers <- List
        .empty[StakerInitializerWithQuantity]
        .tailRecM(current =>
          readLowercasedChoice("Add another staker?")(List("y", "n"), "n".some).flatMap {
            case "y"      => readStaker.map(current.appended).map(_.asLeft[List[StakerInitializerWithQuantity]])
            case "n" | "" => StageResultT.liftF(current.asRight[List[StakerInitializerWithQuantity]].pure[F])
            case _        => writeMessage[F]("Invalid input").as(current.asLeft[List[StakerInitializerWithQuantity]])
          }
        )
    } yield staker0 :: otherStakers

  private val readUnstakedTopl: StageResultT[F, UnspentTransactionOutput] =
    for {
      quantity    <- (writeMessage[F]("Enter a quantity of TOPLs.") >> readQuantity).untilDefinedM
      lockAddress <- readLockAddress
      utxo = UnspentTransactionOutput(
        lockAddress,
        Value.defaultInstance.withTopl(Value.TOPL.defaultInstance.withQuantity(quantity))
      )
    } yield utxo

  private val readUnstakedTopls: StageResultT[F, List[UnspentTransactionOutput]] =
    readLowercasedChoice[F]("Do you want to add an unstaked TOPL?")(List("y", "n"), "n".some)
      .map(_ == "y")
      .ifM(
        for {
          utxo0 <- readUnstakedTopl
          otherUtxos <- List
            .empty[UnspentTransactionOutput]
            .tailRecM(current =>
              readLowercasedChoice[F]("Add another unstaked Topl?")(List("y", "n"), "n".some)
                .flatMap {
                  case "y"      => readUnstakedTopl.map(current.appended).map(_.asLeft[List[UnspentTransactionOutput]])
                  case "n" | "" => StageResultT.liftF(current.asRight[List[UnspentTransactionOutput]].pure[F])
                  case _        => writeMessage[F]("Invalid input").as(current.asLeft[List[UnspentTransactionOutput]])
                }
            )
        } yield utxo0 :: otherUtxos,
        StageResultT.liftF(List.empty[UnspentTransactionOutput].pure[F])
      )

  private val readLvl: StageResultT[F, UnspentTransactionOutput] =
    for {
      quantity    <- (writeMessage[F]("Enter a quantity of LVLs.") >> readQuantity).untilDefinedM
      lockAddress <- readLockAddress
      utxo = UnspentTransactionOutput(
        lockAddress,
        Value.defaultInstance.withLvl(Value.LVL.defaultInstance.withQuantity(quantity))
      )
    } yield utxo

  private val readLvls: StageResultT[F, List[UnspentTransactionOutput]] =
    readLowercasedChoice[F]("Do you want to add a LVL?")(List("y", "n"), "n".some)
      .map(_ == "y")
      .ifM(
        for {
          utxo0 <- readLvl
          otherUtxos <- List
            .empty[UnspentTransactionOutput]
            .tailRecM(current =>
              readLowercasedChoice[F]("Add another LVL?")(List("y", "n"), "n".some)
                .flatMap {
                  case "y"      => readLvl.map(current.appended).map(_.asLeft[List[UnspentTransactionOutput]])
                  case "n" | "" => StageResultT.liftF(current.asRight[List[UnspentTransactionOutput]].pure[F])
                  case _        => writeMessage[F]("Invalid input").as(current.asLeft[List[UnspentTransactionOutput]])
                }
            )
        } yield utxo0 :: otherUtxos,
        StageResultT.liftF(List.empty[UnspentTransactionOutput].pure[F])
      )

  private val readTimestamp =
    (writeMessage[F](
      "Enter a genesis timestamp (milliseconds since UNIX epoch).  Leave blank to use \"now() + 20 seconds\"."
    ) >>
      readInput[F].flatMap {
        case "" =>
          import scala.concurrent.duration._
          StageResultT.liftF(Async[F].realTime.map(_ + 20.seconds).map(_.toMillis.some))
        case str =>
          OptionT
            .fromOption[StageResultT[F, *]](str.toLongOption)
            .flatTapNone(writeMessage[F]("Invalid timestamp"))
            .value
      }).untilDefinedM

  private val readProtocolSettings = {
    implicit val showRatio: Show[co.topl.models.utility.Ratio] = r => s"${r.numerator}/${r.denominator}"
    readLowercasedChoice("Do you want to customize the protocol settings?")(List("y", "n"), "n".some).flatMap {
      case "n" => StageResultT.liftF(DefaultUpdateProposal.pure[F])
      case "y" =>
        for {
          fEffective <- readDefaultedOptional[F, Ratio](
            "f-effective",
            List("1/5", "15/100", "1"),
            DefaultProtocol.fEffective.show
          )
          vrfLddCutoff <- readDefaultedOptional[F, Int]("vrf-ldd-cutoff", List("50"), DefaultProtocol.vrfLddCutoff.show)
          vrfPrecision <- readDefaultedOptional[F, Int]("vrf-precision", List("40"), DefaultProtocol.vrfPrecision.show)
          vrfBaselineDifficulty <- readDefaultedOptional[F, Ratio](
            "vrf-baseline-difficulty",
            List("1/20", "1/50"),
            DefaultProtocol.vrfBaselineDifficulty.show
          )
          vrfAmplitude <- readDefaultedOptional[F, Ratio](
            "vrf-amplitude",
            List("1/2"),
            DefaultProtocol.vrfAmplitude.show
          )
          chainSelectionKLookback <- readDefaultedOptional[F, Long](
            "chain-selection-k-lookback",
            List("500"),
            DefaultProtocol.chainSelectionKLookback.show
          )
          slotDuration <- readDefaultedOptional[F, Duration](
            "slot-duration",
            List("1000 milli"),
            DefaultProtocol.slotDuration.show
          )
          forwardBiasedSlotWindow <- readDefaultedOptional[F, Long](
            "forward-biased-slot-window",
            List("50"),
            DefaultProtocol.forwardBiasedSlotWindow.show
          )
          operationalPeriodsPerEpoch <- readDefaultedOptional[F, Long](
            "operational-periods-per-epoch",
            List("2"),
            DefaultProtocol.operationalPeriodsPerEpoch.show
          )
          kesKeyHours <- readDefaultedOptional[F, Int]("kes-key-hours", List("9"), DefaultProtocol.kesKeyHours.show)
          kesKeyMinutes <- readDefaultedOptional[F, Int](
            "kes-key-minutes",
            List("9"),
            DefaultProtocol.kesKeyMinutes.show
          )
        } yield UpdateProposal(
          "genesis",
          fEffective.some,
          vrfLddCutoff.some,
          vrfPrecision.some,
          vrfBaselineDifficulty.some,
          vrfAmplitude.some,
          chainSelectionKLookback.some,
          slotDuration.some,
          forwardBiasedSlotWindow.some,
          operationalPeriodsPerEpoch.some,
          kesKeyHours.some,
          kesKeyMinutes.some
        )
    }
  }

  private def readOutputDirectory(genesisBlock: FullBlock) =
    writeMessage[F]("Enter a save directory.  Leave blank to use a temporary directory.") >>
    readInput[F].semiflatMap {
      case "" =>
        Files.forAsync[F].createTempDirectory(None, s"testnet-${genesisBlock.header.id.show}", None)
      case str =>
        Path(str).pure[F].flatTap(Files.forAsync[F].createDirectories)
    }

  private def saveGenesisBlock(dir: Path)(block: FullBlock) =
    StageResultT
      .liftF(Sync[F].delay(block.header.id.show))
      .flatMap(stringifiedHeaderId =>
        writeFile[F](dir)(block.header.toByteArray)("Genesis Header", s"$stringifiedHeaderId.header.pbuf") >>
        writeFile[F](dir)(block.header.toProtoString.getBytes(StandardCharsets.UTF_8))(
          "Genesis Header (Proto String)",
          s"$stringifiedHeaderId.header.pstring"
        ) >>
        StageResultT
          .liftF(
            Sync[F].delay(
              BlockBody(block.fullBody.transactions.map(_.id), block.fullBody.rewardTransaction.map(_.id))
            )
          )
          .flatMap(body =>
            writeFile[F](dir)(body.toByteArray)("Genesis Body", s"$stringifiedHeaderId.body.pbuf") >>
            writeFile[F](dir)(body.toProtoString.getBytes(StandardCharsets.UTF_8))(
              "Genesis Body (Proto String)",
              s"$stringifiedHeaderId.body.pstring"
            )
          ) >>
        (block.fullBody.transactions ++ block.fullBody.rewardTransaction).traverse { tx =>
          val stringifiedTxId = tx.id.show
          writeFile[F](dir)(tx.toByteArray)(
            s"Transaction $stringifiedTxId",
            s"$stringifiedTxId.transaction.pbuf"
          ) >>
          writeFile[F](dir)(tx.toProtoString.getBytes(StandardCharsets.UTF_8))(
            s"Transaction $stringifiedTxId (Proto String)",
            s"$stringifiedTxId.transaction.pstring"
          )
        }
      )

  private def saveStaker(dir: Path, name: String)(staker: StakerInitializerWithQuantity) =
    for {
      _ <- writeFile(dir / name)(staker.initializer.operatorSK.toByteArray)(
        s"Operator SK ($name)",
        StakingInit.OperatorKeyName
      )
      _ <- writeFile(dir / name)(staker.initializer.vrfSK.toByteArray)(s"VRF SK ($name)", StakingInit.VrfKeyName)
      _ <- writeFile(dir / name / StakingInit.KesDirectoryName)(
        persistableKesProductSecretKey.persistedBytes(staker.initializer.kesSK).toByteArray
      )(s"KES SK ($name)", "0")
      _ <- writeFile(dir / name)(staker.transaction.toByteArray)(
        s"Registration Transaction ($name)",
        StakingInit.RegistrationTxName
      )
    } yield ()

  private def outro(dir: Path, genesisId: BlockId): StageResultT[F, Unit] =
    writeMessage[F](s"The testnet has been initialized.") >> writeMessage[F](
      s"Each of the stakers in ${dir / "stakers"} should be copied to the corresponding node/machine."
    ) >> writeMessage[F](
      s"The node should also be reconfigured by adding the following lines to your YAML config:"
    ) >>
    writeMessage[F]("bifrost:") >>
    writeMessage[F]("  big-bang:") >>
    writeMessage[F]("    type: public") >>
    writeMessage[F](s"    genesis-id: ${genesisId.show}") >>
    writeMessage[F](s"    source-path: ${dir / "genesis"}")

  val command: StageResultT[F, Unit] =
    for {
      _                <- intro
      stakers          <- readStakers
      unstakedTopls    <- readUnstakedTopls
      lvls             <- readLvls
      protocolSettings <- readProtocolSettings
      timestamp        <- readTimestamp
      protocolUtxo = UnspentTransactionOutput(
        PrivateTestnet.HeightLockOneSpendingAddress,
        Value.defaultInstance.withUpdateProposal(protocolSettings)
      )
      tokenTransaction =
        IoTransaction(
          outputs = protocolUtxo :: unstakedTopls ++ lvls,
          datum = Datum.IoTransaction(
            Event.IoTransaction(Schedule(timestamp = timestamp), metadata = SmallData.defaultInstance)
          )
        )
      genesisConfig = BigBang.Config(
        timestamp,
        stakers.map(_.transaction) :+ tokenTransaction,
        protocolVersion = ProtocolVersioner(appConfig.bifrost.protocols).appVersion.asProtocolVersion
      )
      genesisBlock = BigBang.fromConfig(genesisConfig)
      outputDirectory <- readOutputDirectory(genesisBlock)
      _               <- saveGenesisBlock(outputDirectory / "genesis")(genesisBlock)
      _ <- stakers.traverse { staker =>
        val name = staker.initializer.stakingAddress.show
        saveStaker(outputDirectory / "stakers", name)(staker)
      }
      _ <- outro(outputDirectory, genesisBlock.header.id)
    } yield ()
}

private[cli] case class StakerInitializerWithQuantity(
  initializer: StakerInitializers.Operator,
  quantity:    Int128,
  transaction: IoTransaction
)

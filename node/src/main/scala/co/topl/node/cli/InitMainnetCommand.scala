package co.topl.node.cli

import cats.data.{EitherT, OptionT}
import cats.effect.std.Console
import cats.effect.{Async, Sync}
import cats.implicits._
import cats.{MonadThrow, Show}
import co.topl.blockchain.BigBang
import co.topl.brambl.constants.NetworkConstants
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.box.Value.UpdateProposal
import co.topl.brambl.models.transaction.{IoTransaction, Schedule, UnspentTransactionOutput}
import co.topl.brambl.models.{Datum, Event, LockAddress, LockId}
import co.topl.brambl.syntax._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.config.ApplicationConfig
import co.topl.consensus.models.BlockId
import co.topl.node.ProtocolVersioner
import co.topl.node.models.{BlockBody, FullBlock}
import co.topl.typeclasses.implicits._
import com.google.protobuf.ByteString
import com.google.protobuf.duration.Duration
import fs2.io.file.{Files, Path}
import quivr.models.{Int128, Ratio, SmallData}

import java.nio.charset.StandardCharsets
import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalDateTime}
import scala.util.Try

object InitMainnetCommand {

  def apply[F[_]: Async: Console](appConfig: ApplicationConfig): StageResultT[F, Unit] =
    new InitMainnetCommandImpl[F](appConfig).command

  private[cli] val UnspendableLockAddress = LockAddress(
    network = NetworkConstants.MAIN_NETWORK_ID,
    ledger = NetworkConstants.MAIN_LEDGER_ID,
    id = LockId(ByteString.copyFrom(new Array[Byte](32)))
  )

}

class InitMainnetCommandImpl[F[_]: Async](appConfig: ApplicationConfig)(implicit c: Console[F]) {

  private val intro: StageResultT[F, Unit] =
    writeMessage[F](
      "This utility initializes genesis for a public, decentralized network." +
      "  The output is several files representing the genesis block of the network."
    )

  private def validateRegistrationTransactions(
    transactions: List[IoTransaction]
  ): StageResultT[F, Option[List[IoTransaction]]] =
    if (transactions.isEmpty)
      writeMessage[F]("No transactions found").as(none[List[IoTransaction]])
    else if (transactions.count(_.outputs.exists(_.value.value.topl.exists(_.registration.nonEmpty))) == 0)
      writeMessage[F]("No registration transactions found").as(none[List[IoTransaction]])
    else
      transactions
        .traverse(tx =>
          tx.outputs.traverse(o =>
            Either.cond(
              o.address.network == NetworkConstants.MAIN_NETWORK_ID,
              (),
              show"Invalid LockAddress network for transactionId=${tx.id}"
            )
          )
        )
        .toEitherT[StageResultT[F, *]]
        .leftSemiflatTap(writeMessage[F])
        .toOption
        .as(transactions)
        .value

  private val readRegistrationTransactions: StageResultT[F, List[IoTransaction]] =
    (writeMessage[F]("Enter the directory containing the staker registration transactions.") >>
      readInput[F].flatMap {
        case "" => writeMessage[F]("Invalid directory.").as(none[List[IoTransaction]])
        case str =>
          val path = Path(str)
          StageResultT
            .liftF(Files.forAsync[F].isDirectory(path))
            .ifM(
              StageResultT
                .liftF(
                  Files
                    .forAsync[F]
                    .list(path)
                    .evalMap(Files.forAsync[F].readAll(_).compile.to(Array).map(IoTransaction.parseFrom))
                    .compile
                    .toList
                )
                .flatMap(validateRegistrationTransactions),
              writeMessage[F]("Not a directory.").as(none[List[IoTransaction]])
            )
      }).untilDefinedM

  /**
   * Read a lock address (or retry until a valid value is provided)
   */
  private val readLockAddress =
    (writeMessage[F]("Enter a LockAddress.") >>
      readInput[F].semiflatMap { lockAddressStr =>
        EitherT
          .fromEither[F](co.topl.brambl.codecs.AddressCodecs.decodeAddress(lockAddressStr))
          .ensure("Incorrect network ID")(_.network == NetworkConstants.MAIN_NETWORK_ID)
          .leftSemiflatTap(error => c.println(s"Invalid Lock Address. reason=$error input=$lockAddressStr"))
          .toOption
          .value
      }).untilDefinedM

  /**
   * Read an Int128 quantity (or retry until a valid value is provided)
   */
  private val readQuantity =
    readLowercasedInput.semiflatMap { str =>
      EitherT(MonadThrow[F].catchNonFatal(BigInt(str)).attempt)
        .leftMap(_ => s"Invalid Quantity. input=$str")
        .ensure(s"Quantity too large. input=$str")(_.bitLength <= 128)
        .ensure(s"Quantity must be positive. input=$str")(_ > 0)
        .map(quantity => Int128(ByteString.copyFrom(quantity.toByteArray)))
        .leftSemiflatTap(c.println(_))
        .toOption
        .value
    }

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

  private val readTimestamp = {
    import scala.concurrent.duration._
    (writeMessage[F](
      "Enter a genesis timestamp (milliseconds since UNIX epoch) or a relative duration (i.e. 4 hours).  Leave blank to start in 20 seconds."
    ) >>
    readInput[F].flatMap {
      case "" =>
        StageResultT.liftF(Async[F].realTime.map(_ + 20.seconds).map(_.some))
      case str =>
        OptionT
          .fromOption[StageResultT[F, *]](Try(scala.concurrent.duration.Duration(str)).toOption)
          .semiflatMap(duration => StageResultT.liftF(Async[F].realTime.map(_ + duration)))
          .orElse(
            OptionT
              .fromOption[StageResultT[F, *]](str.toLongOption)
              .flatTapNone(writeMessage[F]("Invalid timestamp"))
              .map(_.milli)
          )
          .collect { case fd: scala.concurrent.duration.FiniteDuration => fd }
          // Double-check with the user that the genesis date/time is correct
          .filterF(duration =>
            StageResultT
              .liftF(
                Sync[F].delay(
                  DateTimeFormatter
                    .ofPattern("MM/dd/yyy HH:mm:ss")
                    .format(
                      LocalDateTime
                        .ofInstant(Instant.ofEpochMilli(duration.toMillis), java.time.Clock.systemDefaultZone().getZone)
                    )
                )
              )
              .flatMap(formattedDateTime =>
                readLowercasedChoice(s"The blockchain will begin on $formattedDateTime. Continue?")(
                  List("y", "n"),
                  "y".some
                )
                  .map(_ == "y")
              )
          )
          .value
    }).untilDefinedM
  }

  private val readProtocolSettings = {
    implicit val showRatio: Show[co.topl.models.utility.Ratio] = r => s"${r.numerator}/${r.denominator}"
    readYesNo("Do you want to customize the protocol settings?", No.some)(
      ifYes = StageResultT.liftF(DefaultUpdateProposal.pure[F]),
      ifNo =
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
    )
  }

  private def readOutputDirectory(genesisBlock: FullBlock) =
    writeMessage[F]("Enter a save directory.  Leave blank to use a temporary directory.") >>
    readInput[F].semiflatMap {
      case "" =>
        Files.forAsync[F].createTempDirectory(None, s"mainnet-${genesisBlock.header.id.show}", None)
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

  private def saveConfig(dir: Path, genesisId: BlockId): StageResultT[F, Unit] =
    for {
      configContents <-
        show"""bifrost:
          |  big-bang:
          |    type: public
          |    genesis-id: $genesisId
          |    source-path: $dir
          |""".stripMargin.pure[StageResultT[F, *]]
      _ <- writeFile(dir)(configContents.getBytes(StandardCharsets.UTF_8))("Config File", "config.yaml")
    } yield ()

  private def outro(dir: Path): StageResultT[F, Unit] =
    writeMessage[F](s"The mainnet genesis block has been initialized.") >>
    writeMessage[F](s"The contents of $dir should be uploaded to a public location, like GitHub.") >>
    writeMessage[F](s"The node can be launched by passing the following argument at launch: --config $dir/config.yaml")

  val command: StageResultT[F, Unit] =
    for {
      _                        <- intro
      registrationTransactions <- readRegistrationTransactions
      unstakedTopls            <- readUnstakedTopls
      lvls                     <- readLvls
      protocolSettings         <- readProtocolSettings
      timestamp                <- readTimestamp.map(_.toMillis)
      protocolUtxo = UnspentTransactionOutput(
        InitMainnetCommand.UnspendableLockAddress,
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
        registrationTransactions :+ tokenTransaction,
        protocolVersion = ProtocolVersioner(appConfig.bifrost.protocols).appVersion.asProtocolVersion
      )
      genesisBlock = BigBang.fromConfig(genesisConfig)
      outputDirectory <- readOutputDirectory(genesisBlock)
      _               <- saveGenesisBlock(outputDirectory)(genesisBlock)
      _               <- saveConfig(outputDirectory, genesisBlock.header.id)
      _               <- outro(outputDirectory)
    } yield ()
}

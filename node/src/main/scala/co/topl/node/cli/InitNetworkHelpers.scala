package co.topl.node.cli

import cats.data._
import cats.effect._
import cats.effect.std.Console
import cats.implicits._
import cats.MonadThrow
import co.topl.blockchain._
import co.topl.brambl.models.LockAddress
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.box.Value.UpdateProposal
import co.topl.brambl.models.transaction.UnspentTransactionOutput
import co.topl.brambl.syntax._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.consensus.models.BlockId
import co.topl.node.models.{BlockBody, FullBlock}
import co.topl.typeclasses.implicits._
import com.google.protobuf.ByteString
import com.google.protobuf.duration.Duration
import fs2.io.file.Path
import quivr.models.{Int128, Ratio}

import java.nio.charset.StandardCharsets
import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalDateTime}
import scala.concurrent.duration.FiniteDuration
import scala.util.Try

object InitNetworkHelpers {

  /**
   * Read a lock address (or retry until a valid value is provided)
   */
  def readLockAddress[F[_]: Sync: Console]: StageResultT[F, LockAddress] =
    (writeMessage[F]("Enter a LockAddress.  Leave blank to use HeightLock=1.") >>
      readInput[F].flatMap {
        case "" =>
          StageResultT.liftF(PrivateTestnet.HeightLockOneSpendingAddress.some.pure[F])
        case lockAddressStr =>
          EitherT
            .fromEither[StageResultT[F, *]](co.topl.brambl.codecs.AddressCodecs.decodeAddress(lockAddressStr))
            .leftSemiflatTap(error => writeMessage(s"Invalid Lock Address. reason=$error input=$lockAddressStr"))
            .toOption
            .value
      }).untilDefinedM

  /**
   * Read an Int128 quantity (or retry until a valid value is provided)
   */
  def readQuantity[F[_]: Sync: Console]: StageResultT[F, Option[Int128]] =
    readLowercasedInput.flatMap {
      case "" =>
        StageResultT.liftF(PrivateTestnet.DefaultTotalStake.some.pure[F])
      case str =>
        EitherT(MonadThrow[StageResultT[F, *]].catchNonFatal(BigInt(str)).attempt)
          .leftMap(_ => s"Invalid Quantity. input=$str")
          .ensure(s"Quantity too large. input=$str")(_.bitLength <= 128)
          .ensure(s"Quantity must be positive. input=$str")(_ > 0)
          .map(quantity => Int128(ByteString.copyFrom(quantity.toByteArray)))
          .leftSemiflatTap(writeMessage[F])
          .toOption
          .value
    }

  def readUnstakedTopl[F[_]: Sync: Console]: StageResultT[F, UnspentTransactionOutput] =
    for {
      quantity    <- (writeMessage[F]("Enter a quantity of TOPLs.") >> readQuantity).untilDefinedM
      lockAddress <- readLockAddress
      utxo = UnspentTransactionOutput(
        lockAddress,
        Value.defaultInstance.withTopl(Value.TOPL.defaultInstance.withQuantity(quantity))
      )
    } yield utxo

  def readUnstakedTopls[F[_]: Sync: Console]: StageResultT[F, List[UnspentTransactionOutput]] =
    readYesNo[F, List[UnspentTransactionOutput]]("Do you want to add an unstaked TOPL?", No.some)(
      ifYes = for {
        utxo0 <- readUnstakedTopl
        otherUtxos <- List
          .empty[UnspentTransactionOutput]
          .tailRecM(current =>
            readYesNo("Add another unstaked Topl?", No.some)(
              ifYes = readUnstakedTopl.map(current.appended).map(_.asLeft[List[UnspentTransactionOutput]]),
              ifNo = StageResultT.liftF(current.asRight[List[UnspentTransactionOutput]].pure[F])
            )
          )
      } yield utxo0 :: otherUtxos,
      ifNo = StageResultT.liftF(List.empty[UnspentTransactionOutput].pure[F])
    )

  def readLvl[F[_]: Sync: Console]: StageResultT[F, UnspentTransactionOutput] =
    for {
      quantity    <- (writeMessage[F]("Enter a quantity of LVLs.") >> readQuantity).untilDefinedM
      lockAddress <- readLockAddress
      utxo = UnspentTransactionOutput(
        lockAddress,
        Value.defaultInstance.withLvl(Value.LVL.defaultInstance.withQuantity(quantity))
      )
    } yield utxo

  def readLvls[F[_]: Sync: Console]: StageResultT[F, List[UnspentTransactionOutput]] =
    readYesNo[F, List[UnspentTransactionOutput]]("Do you want to add a LVL?", No.some)(
      ifYes = for {
        utxo0 <- readLvl
        otherUtxos <- List
          .empty[UnspentTransactionOutput]
          .tailRecM(current =>
            readYesNo("Add another LVL?", No.some)(
              ifYes = readLvl.map(current.appended).map(_.asLeft[List[UnspentTransactionOutput]]),
              ifNo = StageResultT.liftF(current.asRight[List[UnspentTransactionOutput]].pure[F])
            )
          )
      } yield utxo0 :: otherUtxos,
      ifNo = StageResultT.liftF(List.empty[UnspentTransactionOutput].pure[F])
    )

  def readTimestamp[F[_]: Async: Console]: StageResultT[F, FiniteDuration] = {
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
                readYesNo(s"The blockchain will begin on $formattedDateTime. Continue?", Yes.some)(
                  ifYes = StageResultT.liftF(true.pure[F]),
                  ifNo = StageResultT.liftF(false.pure[F])
                )
              )
          )
          .value
    }).untilDefinedM
  }

  def readProtocolSettings[F[_]: Sync: Console]: StageResultT[F, UpdateProposal] =
    readYesNo("Do you want to customize the protocol settings?", No.some)(
      ifYes =
        for {
          fEffective <- readDefaultedOptional[F, Ratio](
            "f-effective",
            List("1/5", "15/100", "1"),
            PublicTestnet.DefaultProtocol.fEffective.show
          )
          vrfLddCutoff <- readDefaultedOptional[F, Int](
            "vrf-ldd-cutoff",
            List("50"),
            PublicTestnet.DefaultProtocol.vrfLddCutoff.show
          )
          vrfPrecision <- readDefaultedOptional[F, Int](
            "vrf-precision",
            List("40"),
            PublicTestnet.DefaultProtocol.vrfPrecision.show
          )
          vrfBaselineDifficulty <- readDefaultedOptional[F, Ratio](
            "vrf-baseline-difficulty",
            List("1/20", "1/50"),
            PublicTestnet.DefaultProtocol.vrfBaselineDifficulty.show
          )
          vrfAmplitude <- readDefaultedOptional[F, Ratio](
            "vrf-amplitude",
            List("1/2"),
            PublicTestnet.DefaultProtocol.vrfAmplitude.show
          )
          chainSelectionKLookback <- readDefaultedOptional[F, Long](
            "chain-selection-k-lookback",
            List("500"),
            PublicTestnet.DefaultProtocol.chainSelectionKLookback.show
          )
          slotDuration <- readDefaultedOptional[F, Duration](
            "slot-duration",
            List("1000 milli"),
            PublicTestnet.DefaultProtocol.slotDuration.show
          )
          forwardBiasedSlotWindow <- readDefaultedOptional[F, Long](
            "forward-biased-slot-window",
            List("50"),
            PublicTestnet.DefaultProtocol.forwardBiasedSlotWindow.show
          )
          operationalPeriodsPerEpoch <- readDefaultedOptional[F, Long](
            "operational-periods-per-epoch",
            List("2"),
            PublicTestnet.DefaultProtocol.operationalPeriodsPerEpoch.show
          )
          kesKeyHours <- readDefaultedOptional[F, Int](
            "kes-key-hours",
            List("9"),
            PublicTestnet.DefaultProtocol.kesKeyHours.show
          )
          kesKeyMinutes <- readDefaultedOptional[F, Int](
            "kes-key-minutes",
            List("9"),
            PublicTestnet.DefaultProtocol.kesKeyMinutes.show
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
        ),
      ifNo = StageResultT.liftF(PublicTestnet.DefaultUpdateProposal.pure[F])
    )

  def saveGenesisBlock[F[_]: Async: Console](dir: Path)(block: FullBlock): StageResultT[F, Unit] =
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
      .void

  def saveConfig[F[_]: Async: Console](dir: Path, genesisId: BlockId): StageResultT[F, Unit] =
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
}

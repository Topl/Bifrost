package co.topl.node.cli

import cats.MonadThrow
import cats.data.{EitherT, OptionT}
import cats.effect.std.{Console, SecureRandom}
import cats.effect.{Async, Sync}
import cats.implicits._
import co.topl.blockchain.{BigBang, PrivateTestnet, StakerInitializers, StakingInit}
import co.topl.brambl.models.{Datum, Event}
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.transaction.{IoTransaction, Schedule, UnspentTransactionOutput}
import co.topl.brambl.syntax._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.config.ApplicationConfig
import co.topl.crypto.hash.Blake2b256
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Sized
import co.topl.node.ProtocolVersioner
import co.topl.node.models.{BlockBody, FullBlock}
import co.topl.typeclasses.implicits._
import com.google.protobuf.ByteString
import fs2.io.file.{Files, Path}
import quivr.models.{Int128, SmallData}

import java.nio.charset.StandardCharsets

object InitTestnetCommand {

  def apply[F[_]: Async: Console](appConfig: ApplicationConfig): StageResultT[F, Unit] =
    new InitTestnetCommandImpl[F](appConfig).command

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
        Sized.strictUnsafe(ByteString.copyFrom(seed)),
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
        writeFile[F](dir)(
          BlockBody(block.fullBody.transactions.map(_.id), block.fullBody.rewardTransaction.map(_.id)).toByteArray
        )("Genesis Body", s"$stringifiedHeaderId.body.pbuf") >>
        (block.fullBody.transactions ++ block.fullBody.rewardTransaction).traverse { tx =>
          val stringifiedTxId = tx.id.show
          writeFile[F](dir)(tx.toByteArray)(s"Transaction $stringifiedTxId", s"$stringifiedTxId.transaction.pbuf")
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

  private def outro(dir: Path): StageResultT[F, Unit] =
    writeMessage[F](s"The testnet has been initialized.") >> writeMessage[F](
      s"Each of the stakers in ${dir / "stakers"} should be copied to the corresponding node/machine."
    ) >> writeMessage[F](
      s"The directory ${dir / "genesis"} can either be referenced in the application setting `bifrost.big-bang.source-path`, or it can be uploaded to GitHub."
    )

  val command: StageResultT[F, Unit] =
    for {
      _             <- intro
      stakers       <- readStakers
      unstakedTopls <- readUnstakedTopls
      lvls          <- readLvls
      timestamp     <- readTimestamp
      tokenTransaction =
        Option.when(unstakedTopls.nonEmpty || lvls.nonEmpty)(
          IoTransaction(
            outputs = unstakedTopls ++ lvls,
            datum = Datum.IoTransaction(
              Event.IoTransaction(Schedule(timestamp = timestamp), metadata = SmallData.defaultInstance)
            )
          )
        )
      genesisConfig = BigBang.Config(
        timestamp,
        stakers.map(_.transaction) ++ tokenTransaction,
        protocolVersion = ProtocolVersioner(appConfig.bifrost.protocols).appVersion.asProtocolVersion
      )
      genesisBlock = BigBang.fromConfig(genesisConfig)
      outputDirectory <- readOutputDirectory(genesisBlock)
      _               <- saveGenesisBlock(outputDirectory / "genesis")(genesisBlock)
      _ <- stakers.traverse { staker =>
        val name = staker.initializer.stakingAddress.show
        saveStaker(outputDirectory / "stakers", name)(staker)
      }
      _ <- outro(outputDirectory)
    } yield ()
}

private[cli] case class StakerInitializerWithQuantity(
  initializer: StakerInitializers.Operator,
  quantity:    Int128,
  transaction: IoTransaction
)
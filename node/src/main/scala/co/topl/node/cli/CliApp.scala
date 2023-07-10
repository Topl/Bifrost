package co.topl.node.cli

import cats.MonadThrow
import cats.data.EitherT
import cats.effect.{IO, Resource, Sync}
import cats.implicits._
import co.topl.blockchain.StakerInitializers
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.models.{Datum, Event, LockAddress}
import co.topl.config.ApplicationConfig
import co.topl.crypto.generation.EntropyToSeed
import co.topl.crypto.generation.mnemonic.Entropy
import co.topl.crypto.signing.{Ed25519, Ed25519VRF, KesProduct}
import co.topl.node.Args
import com.google.protobuf.ByteString
import fs2.Chunk
import fs2.io.file.{Files, Path}
import quivr.models.Int128

class ConfiguredCliApp(args: Args, appConfig: ApplicationConfig) {
  import cats.effect.std.Console

  type F[+A] = IO[A]

  private val c = Console[F]

  def run: F[Unit] = applicationResource.use_

  private def applicationResource: Resource[F, Unit] =
    c.println("Welcome to the Bifrost CLI").toResource >>
    (readUserCommand >>= handleUserCommand).value.iterateWhile(_ != StageResult.Exit).void.toResource

  private def readUserCommand =
    StageResultT[F, CliApp.Command](
      (
        c.println("Please enter a command. [ QUIT | register ]") >>
        c.print("> ") >>
        c.readLine.map(_.trim.toLowerCase)
      ).flatMap {
        case "" | "quit" =>
          CliApp.Command.Quit.some.widen[CliApp.Command].pure[F]
        case "register" =>
          CliApp.Command.Register.some.widen[CliApp.Command].pure[F]
        case v =>
          c.println(s"Invalid command: `$v`").as(none[CliApp.Command])
      }.untilDefinedM
        .map(StageResult.Success(_))
    )

  private def handleUserCommand(command: CliApp.Command): StageResultT[F, Unit] =
    command match {
      case CliApp.Command.Quit =>
        StageResultT[F, Unit](c.println("Mischief Managed.").as(StageResult.Exit))
      case CliApp.Command.Register =>
        handleRegistrationCommand
    }

  private def handleRegistrationCommand: StageResultT[F, Unit] =
    for {
      _           <- prompt
      _           <- requireBramblCli
      _           <- checkStakingDirConfig
      lockAddress <- readLockAddress
      quantity    <- readQuantity
      operatorKey <- createOperatorKey
      vrfKey      <- createVrfKey
      kesKey      <- createKesKey
      stakerInitializer =
        StakerInitializers.Operator(
          ByteString.copyFrom(operatorKey.bytes),
          lockAddress,
          ByteString.copyFrom(vrfKey._1),
          kesKey._1
        )
      transactionOutputs = stakerInitializer.registrationOutputs(quantity)
      transaction = IoTransaction(datum = Datum.IoTransaction(Event.IoTransaction.defaultInstance))
        .withOutputs(transactionOutputs)
      _ <- write(transaction.toByteArray)("Registration Transaction", "registration.transaction.pbuf")
    } yield StageResult.Menu

  /**
   * Initial instruction message
   */
  private val prompt =
    StageResultT[F, Unit](
      c.println("This tool will guide you through the process of preparing Secret Keys for staking.")
        .as(StageResult.Success(()))
    )

  /**
   * Ask the user if Brambl CLI is installed, or Exit otherwise
   */
  private val requireBramblCli =
    StageResultT[F, Unit](
      c.println(
        "The registration process requires Brambl CLI." +
        "  Please ensure it is installed.  See https://github.com/Topl/brambl-cli for details."
      ) >>
      c.println("Continue? [ Y | n ]") >>
      c.print("> ") >>
      c.readLine
        .map(_.trim.toLowerCase)
        .map {
          case "" | "y" => StageResult.Success(())
          case _        => StageResult.Exit
        }
    )

  /**
   * Ask the user if the configured staking directory is correct, or Exit otherwise
   */
  private val checkStakingDirConfig =
    StageResultT[F, Unit](
      c.println(
        show"This process will save keys to ${appConfig.bifrost.staking.directory}." +
        show" If this is incorrect, please reconfigure the node with the correct directory."
      ) >>
      c.println("Continue? [ Y | n ]") >>
      c.print("> ") >>
      c.readLine
        .map(_.trim.toLowerCase)
        .map {
          case "" | "y" => StageResult.Success(())
          case _        => StageResult.Exit
        }
    )

  /**
   * Read a lock address (or retry until a valid value is provided)
   */
  private val readLockAddress =
    StageResultT[F, LockAddress](
      c.println("Using a wallet (i.e. Brambl CLI), create a new LockAddress.") >>
      (c.println("Please enter your LockAddress.") >>
      c.print("> ") >>
      c.readLine.flatMap(lockAddressStr =>
        EitherT
          .fromEither[F](co.topl.brambl.codecs.AddressCodecs.decodeAddress(lockAddressStr))
          .leftSemiflatTap(error => c.println(s"Invalid Lock Address. reason=$error input=$lockAddressStr"))
          .toOption
          .value
      )).untilDefinedM
        .map(StageResult.Success(_))
    )

  /**
   * Read an Int128 quantity (or retry until a valid value is provided)
   */
  private val readQuantity =
    StageResultT[F, Int128](
      (c.println("How many TOPLs do you want to use for staking?") >>
      c.print("> ") >>
      c.readLine.flatMap(str =>
        EitherT(MonadThrow[F].catchNonFatal(BigInt(str)).attempt)
          .leftMap(_ => s"Invalid Quantity. input=$str")
          .ensure(s"Quantity too large. input=$str")(_.bitLength <= 128)
          .ensure(s"Quantity must be positive. input=$str")(_ > 0)
          .leftSemiflatTap(c.println(_))
          .toOption
          .value
      )).untilDefinedM
        .map(quantity => Int128(ByteString.copyFrom(quantity.toByteArray)))
        .map(StageResult.Success(_))
    )

  /**
   * Generate a random seed
   */
  private val createSeed =
    StageResultT[F, Array[Byte]](
      Sync[F]
        .delay(EntropyToSeed.instances.pbkdf2Sha512(32).toSeed(Entropy.generate(), password = None))
        .map(StageResult.Success(_))
    )

  /**
   * Generate and save an Ed25519 Operator key.
   */
  private val createOperatorKey =
    StageResultT[F, Unit](
      c.println("Generating an Ed25519 Operator SK.  This key determines your StakingAddress.")
        .as(StageResult.Success(()))
    ) >>
    createSeed
      .map(new Ed25519().deriveSecretKeyFromSeed)
      .flatTap(key => write(key.bytes)("Operator SK", "operator-key.ed25519.sk"))

  /**
   * Generate and save a VRF key.
   */
  private val createVrfKey =
    StageResultT[F, Unit](
      c.println("Generating a VRF Key.  This key establishes your \"randomness\" within the blockchain.")
        .as(StageResult.Success(()))
    ) >>
    createSeed
      .map(Ed25519VRF.precomputed().deriveKeyPairFromSeed)
      .flatTap(key => write(key._1)("VRF SK", "operator-key.ed25519.sk"))

  /**
   * Generate and save a KES key.
   */
  private val createKesKey =
    StageResultT[F, Unit](
      c.println("Generating a KES Key.  This key maintains forward-security when used honestly.")
        .as(StageResult.Success(()))
    ) >>
    createSeed
      .map(seed =>
        new KesProduct().createKeyPair(
          seed = seed,
          height = (appConfig.bifrost.protocols(0).kesKeyHours, appConfig.bifrost.protocols(0).kesKeyMinutes),
          0
        )
      )
      .flatTap(key =>
        write(co.topl.codecs.bytes.tetra.instances.persistableKesProductSecretKey.persistedBytes(key._1).toByteArray)(
          "KES SK",
          "kes/0"
        )
      )

  /**
   * Write the given file to disk, and log the operation.  Files are saved to the configured staking directory.
   * @param contents The data to write
   * @param logName The name to use in the log operation
   * @param fileName The name of the file to save (within the staking directory)
   */
  private def write(contents: Array[Byte])(logName: String, fileName: String) =
    StageResultT[F, Unit] {
      val destination = Path(appConfig.bifrost.staking.directory) / fileName
      c.println(show"Writing $logName to $destination") >>
      destination.parent.traverse(Files[F].createDirectories) >>
      fs2.Stream
        .chunk(Chunk.array(contents))
        .through(Files[F].writeAll(destination))
        .compile
        .drain
        .as(StageResult.Success(()))
    }

}

object CliApp {
  sealed abstract class Command

  object Command {
    case object Quit extends Command
    case object Register extends Command
  }
}

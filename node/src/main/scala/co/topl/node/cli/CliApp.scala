package co.topl.node.cli

import cats.MonadThrow
import cats.data.EitherT
import cats.effect.{IO, Resource, Sync}
import cats.implicits._
import co.topl.blockchain.{StakerInitializers, StakingInit}
import co.topl.brambl.models.transaction.Schedule
import co.topl.brambl.models.{Datum, Event, LockAddress}
import co.topl.config.ApplicationConfig
import co.topl.crypto.generation.EntropyToSeed
import co.topl.crypto.generation.mnemonic.Entropy
import co.topl.crypto.signing.{Ed25519, Ed25519VRF, KesProduct}
import co.topl.typeclasses.implicits._
import com.google.protobuf.ByteString
import fs2.Chunk
import fs2.io.file.{Files, Path}
import quivr.models.Int128

class ConfiguredCliApp(appConfig: ApplicationConfig) {
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
        readLowercaseInput
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

  /**
   * Generates four files:
   * - Operator Key (Ed25519) saved at {stakingDir}/operator-key.ed25519.sk
   * - VRF Key (Ed25519VRF) saved at {stakingDir}/vrf-key.ed25519vrf.sk
   * - KES Key (KesProduct) saved at {stakingDir}/kes/0
   * - A protobuf-encoded IoTransaction saved at {stakingDir}/registration.transaction.pbuf
   */
  private def handleRegistrationCommand: StageResultT[F, Unit] =
    for {
      _                 <- intro
      _                 <- requireBramblCli
      _                 <- checkStakingDirConfig
      isExistingNetwork <- askIfExistingNetwork
      lockAddress       <- readLockAddress
      quantity          <- readQuantity
      operatorKey       <- createOperatorKey
      vrfKey            <- createVrfKey
      kesKey            <- createKesKey
      stakerInitializer =
        StakerInitializers.Operator(
          ByteString.copyFrom(operatorKey.bytes),
          lockAddress,
          ByteString.copyFrom(vrfKey._1),
          kesKey._1
        )
      _ <- StageResultT.liftF[F, Unit](
        c.println(show"Your staking address is ${stakerInitializer.registration.address}")
      )
      transaction = stakerInitializer
        .registrationTransaction(quantity)
        .withDatum(
          Datum.IoTransaction(
            Event.IoTransaction.defaultInstance.withSchedule(
              if (isExistingNetwork) Schedule(0L, Long.MaxValue, System.currentTimeMillis())
              else Schedule(0L, 0L, System.currentTimeMillis())
            )
          )
        )
      _ <- write(transaction.toByteArray)("Registration Transaction", StakingInit.RegistrationTxName)
      _ <- finalInstructions(isExistingNetwork)
    } yield StageResult.Menu

  /**
   * Initial instruction message
   */
  private val intro =
    StageResultT.liftF[F, Unit](
      c.println("This tool will guide you through the process of preparing Secret Keys for staking.")
    )

  /**
   * Ask the user if Brambl CLI is installed, or Exit otherwise
   */
  private val requireBramblCli =
    StageResultT
      .liftF[F, Unit](
        c.println(
          "The registration process requires Brambl CLI." +
          "  Please ensure it is installed.  See https://github.com/Topl/brambl-cli for details."
        ) >>
        c.println("Continue? [ Y | n ]")
      )
      .flatMapF(_ =>
        readLowercaseInput
          .map {
            case "" | "y" => StageResult.Success(())
            case _        => StageResult.Menu
          }
      )

  /**
   * Ask the user if the configured staking directory is correct, or Exit otherwise
   */
  private val checkStakingDirConfig =
    StageResultT
      .liftF[F, Unit](
        c.println(
          show"This process will save keys to ${appConfig.bifrost.staking.directory}." +
          show" If this is incorrect, please reconfigure the node with the correct directory."
        ) >>
        c.println("Continue? [ Y | n ]")
      )
      .flatMapF(_ =>
        readLowercaseInput
          .map {
            case "" | "y" => StageResult.Success(())
            case _        => StageResult.Menu
          }
      )

  /**
   * Read a lock address (or retry until a valid value is provided)
   */
  private val readLockAddress =
    StageResultT.liftF[F, LockAddress](
      c.println("Using a wallet (i.e. Brambl CLI), create a new LockAddress.") >>
      (c.println("Please enter your LockAddress.") >>
      readInput.flatMap(lockAddressStr =>
        EitherT
          .fromEither[F](co.topl.brambl.codecs.AddressCodecs.decodeAddress(lockAddressStr))
          .leftSemiflatTap(error => c.println(s"Invalid Lock Address. reason=$error input=$lockAddressStr"))
          .toOption
          .value
      )).untilDefinedM
    )

  /**
   * Read an Int128 quantity (or retry until a valid value is provided)
   */
  private val readQuantity =
    StageResultT.liftF[F, Int128](
      (c.println("How many TOPLs do you want to use for staking?") >>
      readLowercaseInput.flatMap(str =>
        EitherT(MonadThrow[F].catchNonFatal(BigInt(str)).attempt)
          .leftMap(_ => s"Invalid Quantity. input=$str")
          .ensure(s"Quantity too large. input=$str")(_.bitLength <= 128)
          .ensure(s"Quantity must be positive. input=$str")(_ > 0)
          .leftSemiflatTap(c.println(_))
          .toOption
          .value
      )).untilDefinedM
        .map(quantity => Int128(ByteString.copyFrom(quantity.toByteArray)))
    )

  /**
   * Generate a random seed
   */
  private val createSeed =
    StageResultT.liftF[F, Array[Byte]](
      Sync[F]
        .delay(EntropyToSeed.instances.pbkdf2Sha512(32).toSeed(Entropy.generate(), password = None))
    )

  /**
   * Generate and save an Ed25519 Operator key.
   */
  private val createOperatorKey =
    StageResultT.liftF[F, Unit](
      c.println("Generating an Ed25519 Operator SK.  This key determines your StakingAddress.")
    ) >>
    createSeed
      .map(new Ed25519().deriveSecretKeyFromSeed)
      .flatTap(key => write(key.bytes)("Operator SK", StakingInit.OperatorKeyName))

  /**
   * Generate and save a VRF key.
   */
  private val createVrfKey =
    StageResultT.liftF[F, Unit](
      c.println("Generating a VRF Key.  This key establishes your \"randomness\" within the blockchain.")
    ) >>
    createSeed
      .map(Ed25519VRF.precomputed().deriveKeyPairFromSeed)
      .flatTap(key => write(key._1)("VRF SK", StakingInit.VrfKeyName))

  /**
   * Generate and save a KES key.
   */
  private val createKesKey =
    StageResultT.liftF[F, Unit](
      c.println("Generating a KES Key.  This key maintains forward-security when used honestly.")
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
          s"${StakingInit.KesDirectoryName}/0"
        )
      )

  private val askIfExistingNetwork: StageResultT[F, Boolean] =
    StageResultT.liftF[F, Unit](
      c.println("Which type of network are you joining? [ EXISTING | new ]")
    ) >> StageResultT
      .liftF(readLowercaseInput)
      .flatMap {
        case "" | "existing" => StageResultT.liftF(true.pure[F])
        case "new"           => StageResultT.liftF(false.pure[F])
        case _ =>
          StageResultT.liftF(
            c.println("Invalid network type.  In most cases, you should choose \"existing\" unless otherwise directed.")
          ) >> askIfExistingNetwork
      }

  private def finalInstructions(isExistingNetwork: Boolean) =
    StageResultT.liftF(
      c.println(
        if (isExistingNetwork)
          "Your staking keys have been saved.  The Transaction that was saved should be imported into Brambl for input selection and broadcast." +
          "  Once broadcasted, save the updated transaction back to disk (overwrite the previous)." +
          "  Next, you can launch your node, and it will search the chain for your registration Transaction." +
          "  It may take up to two epochs before your node begins producing new blocks."
        else
          "Your staking keys have been saved.  The Transaction that was saved should be submitted to your" +
          " genesis coordinator contact at Topl for further processing."
      )
    )

  /**
   * Prompt a user for input and trim any excess spaces
   */
  private val readInput: F[String] =
    c.print("> ") >> c.readLine.map(_.trim)

  /**
   * Prompt a user for input trim any excess spaces, and lowercase the result
   */
  private val readLowercaseInput: F[String] =
    readInput.map(_.toLowerCase)

  /**
   * Write the given file to disk, and log the operation.  Files are saved to the configured staking directory.
   * @param contents The data to write
   * @param logName The name to use in the log operation
   * @param fileName The name of the file to save (within the staking directory)
   */
  private def write(contents: Array[Byte])(logName: String, fileName: String) =
    StageResultT.liftF[F, Unit] {
      val destination = Path(appConfig.bifrost.staking.directory) / fileName
      c.println(show"Writing $logName to $destination") >>
      destination.parent.traverse(Files[F].createDirectories) >>
      fs2.Stream
        .chunk(Chunk.array(contents))
        .through(Files[F].writeAll(destination))
        .compile
        .drain
    }

}

object CliApp {
  sealed abstract class Command

  object Command {
    case object Quit extends Command
    case object Register extends Command
  }
}

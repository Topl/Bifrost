package co.topl.node.cli

import cats.MonadThrow
import cats.data.EitherT
import cats.effect.std.Console
import cats.effect.{Async, Sync}
import cats.implicits._
import co.topl.blockchain.{StakerInitializers, StakingInit}
import co.topl.brambl.models.{Datum, Event}
import co.topl.brambl.models.transaction.Schedule
import co.topl.codecs.bytes.tetra.instances._
import co.topl.config.ApplicationConfig
import co.topl.crypto.generation.EntropyToSeed
import co.topl.crypto.generation.mnemonic.Entropy
import co.topl.crypto.signing.{Ed25519, Ed25519VRF, KesProduct}
import co.topl.typeclasses.implicits._
import com.google.protobuf.ByteString
import fs2.io.file.Path
import quivr.models.Int128

object RegistrationCommand {

  def apply[F[_]: Async: Console](appConfig: ApplicationConfig): StageResultT[F, Unit] =
    new RegistrationCommandImpl[F](appConfig).command
}

class RegistrationCommandImpl[F[_]: Async](appConfig: ApplicationConfig)(implicit c: Console[F]) {

  private val stakingDirectory = Path(appConfig.bifrost.staking.directory)

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
    writeMessage[F](
      "The registration process requires Brambl CLI." +
      "  Please ensure it is installed.  See https://github.com/Topl/brambl-cli for details."
    ) >>
    readLowercasedChoice[F]("Continue?")(List("y", "n"), Some("y"))
      .subflatMap {
        case "" | "y" => StageResult.success(())
        case _        => StageResult.menu
      }

  /**
   * Ask the user if the configured staking directory is correct, or Exit otherwise
   */
  private val checkStakingDirConfig =
    writeMessage[F](
      show"This process will save keys to ${stakingDirectory}." +
      show" If this is incorrect, please reconfigure the node with the correct directory."
    ) >>
    readLowercasedChoice[F]("Continue?")(List("y", "n"), Some("y"))
      .subflatMap {
        case "" | "y" => StageResult.success(())
        case _        => StageResult.menu
      }

  /**
   * Read a lock address (or retry until a valid value is provided)
   */
  private val readLockAddress =
    writeMessage[F]("Using a wallet (i.e. Brambl CLI), create a new LockAddress.") >>
    (writeMessage[F]("Please enter your LockAddress.") >>
    readInput[F].semiflatMap(lockAddressStr =>
      EitherT
        .fromEither[F](co.topl.brambl.codecs.AddressCodecs.decodeAddress(lockAddressStr))
        .leftSemiflatTap(error => c.println(s"Invalid Lock Address. reason=$error input=$lockAddressStr"))
        .toOption
        .value
    )).untilDefinedM

  /**
   * Read an Int128 quantity (or retry until a valid value is provided)
   */
  private val readQuantity =
    writeMessage[F]("How many TOPLs do you want to use for staking?") >>
    readLowercasedInput
      .semiflatMap(str =>
        EitherT(MonadThrow[F].catchNonFatal(BigInt(str)).attempt)
          .leftMap(_ => s"Invalid Quantity. input=$str")
          .ensure(s"Quantity too large. input=$str")(_.bitLength <= 128)
          .ensure(s"Quantity must be positive. input=$str")(_ > 0)
          .map(quantity => Int128(ByteString.copyFrom(quantity.toByteArray)))
          .leftSemiflatTap(c.println(_))
          .toOption
          .value
      )
      .untilDefinedM

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
    writeMessage[F]("Generating an Ed25519 Operator SK.  This key determines your StakingAddress.") >>
    createSeed
      .map(new Ed25519().deriveSecretKeyFromSeed)
      .flatTap(key => writeFile(stakingDirectory)(key.bytes)("Operator SK", StakingInit.OperatorKeyName))

  /**
   * Generate and save a VRF key.
   */
  private val createVrfKey =
    writeMessage[F]("Generating a VRF Key.  This key establishes your \"randomness\" within the blockchain.") >>
    createSeed
      .map(Ed25519VRF.precomputed().deriveKeyPairFromSeed)
      .flatTap(key => writeFile(stakingDirectory)(key._1)("VRF SK", StakingInit.VrfKeyName))

  /**
   * Generate and save a KES key.
   */
  private val createKesKey =
    writeMessage[F]("Generating a KES Key.  This key maintains forward-security when used honestly.") >>
    createSeed
      .map(seed =>
        new KesProduct().createKeyPair(
          seed = seed,
          height = (appConfig.bifrost.protocols(0).kesKeyHours, appConfig.bifrost.protocols(0).kesKeyMinutes),
          0
        )
      )
      .flatTap(key =>
        writeFile(stakingDirectory / StakingInit.KesDirectoryName)(
          persistableKesProductSecretKey.persistedBytes(key._1).toByteArray
        )("KES SK", "0")
      )

  private val askIfExistingNetwork: StageResultT[F, Boolean] =
    readLowercasedChoice[F]("Which type of network are you joining?")(List("existing", "new"), Some("existing"))
      .flatMap {
        case "" | "existing" => StageResultT.liftF(true.pure[F])
        case "new"           => StageResultT.liftF(false.pure[F])
        case _ =>
          writeMessage[F](
            "Invalid network type.  In most cases, you should choose \"existing\" unless otherwise directed."
          ) >> askIfExistingNetwork
      }

  private def finalInstructions(isExistingNetwork: Boolean) =
    writeMessage[F](
      if (isExistingNetwork)
        "Your staking keys have been saved.  The Transaction that was saved should be imported into Brambl for input selection and broadcast." +
        "  Once broadcasted, save the updated transaction back to disk (overwrite the previous)." +
        "  Next, you can launch your node, and it will search the chain for your registration Transaction." +
        "  It may take up to two epochs before your node begins producing new blocks."
      else
        "Your staking keys have been saved.  The Transaction that was saved should be submitted to your" +
        " genesis coordinator contact at Topl for further processing."
    )

  /**
   * Generates four files:
   * - Operator Key (Ed25519) saved at {stakingDir}/operator-key.ed25519.sk
   * - VRF Key (Ed25519VRF) saved at {stakingDir}/vrf-key.ed25519vrf.sk
   * - KES Key (KesProduct) saved at {stakingDir}/kes/0
   * - A protobuf-encoded IoTransaction saved at {stakingDir}/registration.transaction.pbuf
   */
  val command: StageResultT[F, Unit] =
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
      _ <- writeFile(stakingDirectory)(transaction.toByteArray)(
        "Registration Transaction",
        StakingInit.RegistrationTxName
      )
      _ <- finalInstructions(isExistingNetwork)
    } yield StageResult.Menu
}

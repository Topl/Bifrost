package co.topl.node.cli

import cats.effect.std.{Console, SecureRandom}
import cats.effect.{Async, Sync}
import cats.implicits._
import co.topl.blockchain.{BigBang, PrivateTestnet, StakerInitializers, StakingInit}
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.transaction.{IoTransaction, Schedule, UnspentTransactionOutput}
import co.topl.brambl.models.{Datum, Event}
import co.topl.codecs.bytes.tetra.instances._
import co.topl.config.ApplicationConfig
import co.topl.crypto.hash.Blake2b256
import co.topl.node.ProtocolVersioner
import co.topl.node.models.FullBlock
import co.topl.typeclasses.implicits._
import fs2.io.file.{Files, Path}
import quivr.models.{Int128, SmallData}

import java.nio.charset.StandardCharsets

object InitTestnetCommand {

  def apply[F[_]: Async: Console](appConfig: ApplicationConfig): StageResultT[F, Unit] =
    new InitTestnetCommandImpl[F](appConfig).command

}

class InitTestnetCommandImpl[F[_]: Async: Console](appConfig: ApplicationConfig) {

  import InitNetworkHelpers._

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

  private def readOutputDirectory(genesisBlock: FullBlock) =
    writeMessage[F]("Enter a save directory.  Leave blank to use a temporary directory.") >>
    readInput[F].semiflatMap {
      case "" =>
        Files.forAsync[F].createTempDirectory(None, s"testnet-${genesisBlock.header.id.show}", None)
      case str =>
        Path(str).pure[F].flatTap(Files.forAsync[F].createDirectories)
    }

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
    writeMessage[F](s"The testnet has been initialized.") >>
    writeMessage[F](s"Each of the stakers in ${dir / "stakers"} should be moved to the corresponding node/machine.") >>
    writeMessage[F](s"The node can be launched by passing the following argument at launch: --config $dir/config.yaml")

  val command: StageResultT[F, Unit] =
    for {
      _                <- intro
      stakers          <- readStakers
      unstakedTopls    <- readUnstakedTopls
      lvls             <- readLvls
      protocolSettings <- readProtocolSettings
      timestamp        <- readTimestamp.map(_.toMillis)
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
      _ <- saveConfig(outputDirectory, genesisBlock.header.id)
      _ <- outro(outputDirectory)
    } yield ()
}

private[cli] case class StakerInitializerWithQuantity(
  initializer: StakerInitializers.Operator,
  quantity:    Int128,
  transaction: IoTransaction
)

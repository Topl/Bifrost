package co.topl.node

import cats._
import cats.data.OptionT
import cats.implicits._
import cats.effect.implicits._
import cats.effect._
import co.topl.algebras.ClockAlgebra
import co.topl.blockchain.{CryptoResources, StakerInitializers}
import co.topl.brambl.models.LockAddress
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.codecs.bytes.tetra.instances.persistableKesProductSecretKey
import co.topl.codecs.bytes.typeclasses.Persistable
import co.topl.config.ApplicationConfig
import co.topl.consensus.algebras.{
  ConsensusValidationStateAlgebra,
  EtaCalculationAlgebra,
  LeaderElectionValidationAlgebra
}
import co.topl.consensus.models.{ProtocolVersion, StakingAddress, VrfConfig}
import co.topl.crypto.models.SecretKeyKesProduct
import co.topl.interpreters.CatsSecureStore
import co.topl.minting.algebras.StakingAlgebra
import co.topl.minting.interpreters.{OperationalKeyMaker, Staking, VrfCalculator}
import com.google.protobuf.ByteString
import fs2.Chunk
import fs2.io.file.{Files, Path}
import org.typelevel.log4cats.Logger

object StakingInit {

  final val KesDirectoryName = "kes"
  final val OperatorKeyName = "operator-key.ed25519.sk"
  final val VrfKeyName = "vrf-key.ed25519vrf.sk"
  final val RegistrationTxName = "registration.transaction.pbuf"

  /**
   * Initializes a Staking object from a private genesis configuration.
   */
  def makeStakingFromGenesis[F[_]: Async: Logger](
    stakingDir:               Path,
    initializer:              StakerInitializers.Operator,
    rewardAddress:            LockAddress,
    clock:                    ClockAlgebra[F],
    etaCalculation:           EtaCalculationAlgebra[F],
    consensusValidationState: ConsensusValidationStateAlgebra[F],
    leaderElectionThreshold:  LeaderElectionValidationAlgebra[F],
    cryptoResources:          CryptoResources[F],
    protocol:                 ApplicationConfig.Bifrost.Protocol,
    vrfConfig:                VrfConfig,
    protocolVersion:          ProtocolVersion
  ): Resource[F, StakingAlgebra[F]] =
    for {
      // Initialize a persistent secure store
      kesPath <- Sync[F].delay(stakingDir / "kes").toResource
      _       <- Files[F].createDirectories(kesPath).toResource
      _ <- fs2.Stream
        .chunk(
          Chunk.byteBuffer(Persistable[SecretKeyKesProduct].persistedBytes(initializer.kesSK).asReadOnlyByteBuffer())
        )
        .through(Files[F].writeAll(kesPath / "0"))
        .compile
        .drain
        .toResource
      staking <- makeStaking(
        stakingDir,
        initializer.vrfSK,
        initializer.vrfVK,
        initializer.stakingAddress,
        rewardAddress,
        clock,
        etaCalculation,
        consensusValidationState,
        leaderElectionThreshold,
        cryptoResources,
        protocol,
        vrfConfig,
        protocolVersion
      )
    } yield staking

  /**
   * Initializes a Staking object from existing files on disk.  The files are expected to be in the format created
   * by the "Registration" CLI process.
   */
  def makeStakingFromDisk[F[_]: Async: Logger](
    stakingDir:               Path,
    rewardAddress:            LockAddress,
    clock:                    ClockAlgebra[F],
    etaCalculation:           EtaCalculationAlgebra[F],
    consensusValidationState: ConsensusValidationStateAlgebra[F],
    leaderElectionThreshold:  LeaderElectionValidationAlgebra[F],
    cryptoResources:          CryptoResources[F],
    protocol:                 ApplicationConfig.Bifrost.Protocol,
    vrfConfig:                VrfConfig,
    protocolVersion:          ProtocolVersion
  ): Resource[F, StakingAlgebra[F]] =
    for {
      kesPath <- Sync[F].delay(stakingDir / "kes").toResource
      _ <- Files[F]
        .list(kesPath)
        .compile
        .toList
        .flatMap(files =>
          MonadThrow[F]
            .raiseWhen(files.length != 1)(new IllegalArgumentException("Expected exactly one KES key in secure store."))
        )
        .toResource
      readFile = (p: Path) => Files[F].readAll(p).compile.to(Chunk)
      vrfSK       <- readFile(stakingDir / VrfKeyName).map(_.toArray).toResource
      vrfVK       <- cryptoResources.ed25519VRF.useSync(_.getVerificationKey(vrfSK)).toResource
      transaction <- readFile(stakingDir / RegistrationTxName).map(_.toArray).map(IoTransaction.parseFrom).toResource
      registration <- OptionT
        .fromOption[F](transaction.outputs.headOption.flatMap(_.value.value.topl.flatMap(_.registration)))
        .getOrRaise(new IllegalArgumentException("Registration Transaction is invalid"))
        .toResource
      staking <- makeStaking(
        stakingDir,
        ByteString.copyFrom(vrfSK),
        ByteString.copyFrom(vrfVK),
        registration.address,
        rewardAddress,
        clock,
        etaCalculation,
        consensusValidationState,
        leaderElectionThreshold,
        cryptoResources,
        protocol,
        vrfConfig,
        protocolVersion
      )
    } yield staking

  /**
   * Initializes a Staking object from the given raw VRF and staking address information
   */
  def makeStaking[F[_]: Async: Logger](
    stakingDir:               Path,
    vrfSK:                    ByteString,
    vrfVK:                    ByteString,
    stakingAddress:           StakingAddress,
    rewardAddress:            LockAddress,
    clock:                    ClockAlgebra[F],
    etaCalculation:           EtaCalculationAlgebra[F],
    consensusValidationState: ConsensusValidationStateAlgebra[F],
    leaderElectionThreshold:  LeaderElectionValidationAlgebra[F],
    cryptoResources:          CryptoResources[F],
    protocol:                 ApplicationConfig.Bifrost.Protocol,
    vrfConfig:                VrfConfig,
    protocolVersion:          ProtocolVersion
  ): Resource[F, StakingAlgebra[F]] =
    for {
      kesPath     <- Sync[F].delay(stakingDir / "kes").toResource
      secureStore <- CatsSecureStore.make[F](kesPath.toNioPath)
      vrfCalculator <- VrfCalculator.make[F](
        vrfSK,
        cryptoResources.ed25519VRF,
        protocol.vrfCacheSize
      )

      operationalKeys <- OperationalKeyMaker
        .make[F](
          operationalPeriodLength = protocol.operationalPeriodLength,
          activationOperationalPeriod = 0L, // TODO: Accept registration block as `make` parameter?
          stakingAddress,
          vrfConfig,
          secureStore = secureStore,
          clock = clock,
          vrfCalculator = vrfCalculator,
          leaderElectionThreshold,
          etaCalculation,
          consensusValidationState,
          cryptoResources.kesProduct,
          cryptoResources.ed25519
        )

      staking <- Staking.make(
        stakingAddress,
        rewardAddress,
        vrfVK,
        operationalKeys,
        consensusValidationState,
        etaCalculation,
        cryptoResources.ed25519,
        cryptoResources.blake2b256,
        vrfCalculator,
        leaderElectionThreshold,
        protocolVersion
      )
    } yield staking

}

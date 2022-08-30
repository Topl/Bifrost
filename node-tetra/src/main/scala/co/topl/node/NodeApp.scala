package co.topl.node

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.stream.scaladsl.Source
import akka.util.Timeout
import cats.effect.IO
import co.topl.catsakka._
import cats.implicits._
import co.topl.algebras.{ClockAlgebra, UnsafeResource}
import ClockAlgebra.implicits._
import cats.Applicative
import co.topl.blockchain.{BigBang, Blockchain, PrivateTestnet, StakerInitializers}
import co.topl.catsakka.IOAkkaApp
import co.topl.crypto.hash.{Blake2b256, Blake2b512}
import co.topl.crypto.signing._
import co.topl.interpreters.{
  ActorPoolUnsafeResource,
  AkkaSecureStore,
  BlockHeightTree,
  BlockIdTree,
  SchedulerClock,
  StatsInterpreter
}
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.models._
import co.topl.typeclasses.implicits._
import co.topl.codecs.bytes.tetra.instances._
import com.typesafe.config.ConfigFactory
import co.topl.consensus._
import co.topl.consensus.algebras.{
  ConsensusValidationStateAlgebra,
  EtaCalculationAlgebra,
  LeaderElectionValidationAlgebra
}
import co.topl.consensus.interpreters._
import co.topl.ledger.interpreters._
import co.topl.minting.{LeaderElectionMinting, OperationalKeys, Staking, VrfProof}
import co.topl.networking.p2p.LocalPeer
import co.topl.numerics._
import fs2.io.file.{Files, Path}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.net.InetSocketAddress
import java.security.SecureRandom
import java.time.Instant
import java.util.UUID
import scala.concurrent.duration._
import scala.util.Random

object NodeApp
    extends IOAkkaApp[Args, Nothing](
      createArgs = args => Args.parserArgs.constructOrThrow(args),
      createConfig = args => ConfigFactory.load(),
      createSystem = (args, config) => ActorSystem[Nothing](Behaviors.empty, "BifrostTetra", config)
    ) {

  implicit private val logger: Logger[F] =
    Slf4jLogger.getLoggerFromClass[F](this.getClass)

  implicit private val defaultAskTimeout: Timeout =
    Timeout(10.seconds)

  def run: IO[Unit] =
    (Files[F].tempDirectory, Files[F].tempDirectory).tupled.use { case (dataDir, stakingDir) =>
      for {
        _ <- Logger[F].info(show"Launching node with args=$args")
        _ <- Logger[F].info(show"Using temporary dataDir=$dataDir")
        _ <- Logger[F].info(show"Using temporary stakingDir=$stakingDir")
        // Values are hardcoded for now; other tickets/work will make these configurable
        bigBangTimestamp <- Instant.now().plusSeconds(5).toEpochMilli.pure[F]
        localPeer = LocalPeer(InetSocketAddress.createUnresolved("localhost", 9085), (0, 0))
        rpcHost = "0.0.0.0"
        rpcPort = 9084
        implicit0(networkPrefix: NetworkPrefix) = NetworkPrefix(1: Byte)
        stakerInitializers = PrivateTestnet.stakerInitializers(bigBangTimestamp, 1)
        implicit0(bigBangConfig: BigBang.Config) = PrivateTestnet.config(bigBangTimestamp, stakerInitializers)
        bigBangBlock = BigBang.block
        _ <- Logger[F].info(show"Big Bang Block id=${bigBangBlock.headerV2.id.asTypedBytes}")

        // Initialize crypto resources

        blake2b256Resource <- ActorPoolUnsafeResource.Eval.make[F, Blake2b256](new Blake2b256, _ => ())
        blake2b512Resource <- ActorPoolUnsafeResource.Eval.make[F, Blake2b512](new Blake2b512, _ => ())
        ed25519VRFResource <- ActorPoolUnsafeResource.Eval.make[F, Ed25519VRF](Ed25519VRF.precomputed(), _ => ())
        kesProductResource <- ActorPoolUnsafeResource.Eval.make[F, KesProduct](new KesProduct, _ => ())
        curve25519Resource <- ActorPoolUnsafeResource.Eval.make[F, Curve25519](new Curve25519, _ => ())
        ed25519Resource    <- ActorPoolUnsafeResource.Eval.make[F, Ed25519](new Ed25519, _ => ())
        extendedEd25519Resource <- ActorPoolUnsafeResource.Eval
          .make[F, ExtendedEd25519](ExtendedEd25519.precomputed(), _ => ())

        dataStores <- DataStores.init[F](dataDir)(bigBangBlock)

        blockIdTree <- BlockIdTree.make[F]
        _           <- blockIdTree.associate(bigBangBlock.headerV2.id, bigBangBlock.headerV2.parentHeaderId)

        // Start supporting interpreters
        blockHeightTree <- BlockHeightTree
          .make[F](
            dataStores.blockHeightTree,
            bigBangBlock.headerV2.parentHeaderId,
            dataStores.slotData,
            dataStores.blockHeightTreeUnapply,
            blockIdTree
          )
        clock = SchedulerClock.Eval.make[F](
          ProtocolConfiguration.SlotDuration,
          ProtocolConfiguration.EpochLength,
          Instant.ofEpochMilli(bigBangTimestamp)
        )
        etaCalculation <- EtaCalculation.Eval.make(
          dataStores.slotData.getOrRaise,
          clock,
          bigBangBlock.headerV2.eligibilityCertificate.eta,
          blake2b256Resource,
          blake2b512Resource
        )
        exp   <- ExpInterpreter.make[F](10000, 38)
        log1p <- Log1pInterpreter.make[F](10000, 8).flatMap(Log1pInterpreter.makeCached[F])
        leaderElectionThreshold = LeaderElectionValidation.Eval
          .make[F](ProtocolConfiguration.vrfConfig, blake2b512Resource, exp, log1p)
        epochBoundariesState <- EpochBoundariesEventSourcedState.make[F](
          clock,
          bigBangBlock.headerV2.parentHeaderId.pure[F],
          blockIdTree,
          dataStores.epochBoundaries.pure[F],
          dataStores.slotData.getOrRaise
        )
        consensusDataState <- ConsensusDataEventSourcedState.make[F](
          bigBangBlock.headerV2.parentHeaderId.pure[F],
          blockIdTree,
          ConsensusDataEventSourcedState
            .ConsensusData(dataStores.operatorStakes, dataStores.activeStake, dataStores.registrations)
            .pure[F],
          dataStores.bodies.getOrRaise,
          dataStores.transactions.getOrRaise,
          boxId =>
            dataStores.transactions
              .getOrRaise(boxId.transactionId)
              .map(_.outputs.get(boxId.transactionOutputIndex.toLong).get)
        )
        consensusValidationState <- ConsensusValidationState
          .make[F](bigBangBlock.headerV2.id, epochBoundariesState, consensusDataState, clock)
        headerValidation <- BlockHeaderValidation.Eval
          .make[F](
            etaCalculation,
            consensusValidationState,
            leaderElectionThreshold,
            ed25519VRFResource,
            kesProductResource,
            ed25519Resource,
            blake2b256Resource
          )
          .flatMap(BlockHeaderValidation.WithCache.make[F](_, dataStores.headers))
        localChain <- LocalChain.Eval.make(
          bigBangBlock.headerV2.slotData(Ed25519VRF.precomputed()),
          ChainSelection
            .orderT[F](
              dataStores.slotData.getOrRaise,
              blake2b512Resource,
              ProtocolConfiguration.ChainSelectionKLookback,
              ProtocolConfiguration.ChainSelectionSWindow
            )
        )
        mempool <- Mempool.make[F](
          bigBangBlock.headerV2.parentHeaderId.pure[F],
          dataStores.bodies.getOrRaise,
          dataStores.transactions.getOrRaise,
          blockIdTree,
          clock,
          id => Logger[F].info(show"Expiring transaction id=$id"),
          1000L,
          1000L
        )
        implicit0(networkRandom: Random) = new Random(new SecureRandom())
        boxState <- BoxState.make(
          bigBangBlock.headerV2.parentHeaderId.pure[F],
          dataStores.bodies.getOrRaise,
          dataStores.transactions.getOrRaise,
          blockIdTree,
          dataStores.spendableBoxIds.pure[F]
        )
        transactionSyntaxValidation <- TransactionSyntaxValidation.make[F]
        transactionAuthorizationValidation <- TransactionAuthorizationValidation.make[F](
          blake2b256Resource,
          curve25519Resource,
          ed25519Resource,
          extendedEd25519Resource,
          dataStores.slotData.getOrRaise
        )
        bodySyntaxValidation <- BodySyntaxValidation
          .make[F](dataStores.transactions.getOrRaise, transactionSyntaxValidation)
        bodySemanticValidation <- BodySemanticValidation.make[F](
          dataStores.transactions.getOrRaise,
          boxState,
          boxState => TransactionSemanticValidation.make[F](dataStores.transactions.getOrRaise, boxState)
        )
        bodyAuthorizationValidation <- BodyAuthorizationValidation.make[F](
          dataStores.transactions.getOrRaise,
          transactionAuthorizationValidation
        )
        staking <- makeStaking(
          stakingDir,
          bigBangBlock.headerV2,
          stakerInitializers.headOption.get,
          clock,
          etaCalculation,
          consensusValidationState,
          leaderElectionThreshold,
          ed25519Resource,
          ed25519VRFResource,
          kesProductResource
        )
        // Finally, run the program
        _ <- Blockchain
          .run[F](
            clock,
            staking.some,
            dataStores.slotData,
            dataStores.headers,
            dataStores.bodies,
            dataStores.transactions,
            localChain,
            blockIdTree,
            blockHeightTree,
            headerValidation,
            transactionSyntaxValidation,
            bodySyntaxValidation,
            bodySemanticValidation,
            bodyAuthorizationValidation,
            mempool,
            ed25519VRFResource,
            localPeer,
            Source.never,
            (peer, flow) => flow,
            rpcHost,
            rpcPort
          )
      } yield ()
    }

  private def makeStaking(
    stakingDir:               Path,
    bigBangHeader:            BlockHeaderV2,
    initializer:              StakerInitializers.Operator,
    clock:                    ClockAlgebra[F],
    etaCalculation:           EtaCalculationAlgebra[F],
    consensusValidationState: ConsensusValidationStateAlgebra[F],
    leaderElectionThreshold:  LeaderElectionValidationAlgebra[F],
    ed25519Resource:          UnsafeResource[F, Ed25519],
    ed25519VRFResource:       UnsafeResource[F, Ed25519VRF],
    kesProductResource:       UnsafeResource[F, KesProduct]
  ) =
    for {
      // Initialize a persistent secure store
      secureStore <- AkkaSecureStore.Eval.make[F](stakingDir.toNioPath)
      // Determine if a key has already been initialized
      _ <- secureStore.list
        .map(_.isEmpty)
        // If uninitialized, generate a new key.  Otherwise, move on.
        .ifM(secureStore.write(UUID.randomUUID().toString, initializer.kesSK), Applicative[F].unit)
      vrfProofConstruction <- VrfProof.Eval.make[F](
        initializer.vrfSK,
        clock,
        leaderElectionThreshold,
        ed25519VRFResource,
        ProtocolConfiguration.vrfConfig
      )
      initialSlot  <- clock.globalSlot.map(_.max(0L))
      initialEpoch <- clock.epochOf(initialSlot)
      _            <- vrfProofConstruction.precomputeForEpoch(initialEpoch, bigBangHeader.eligibilityCertificate.eta)
      operationalKeys <- OperationalKeys.FromSecureStore.make[F](
        secureStore = secureStore,
        clock = clock,
        vrfProof = vrfProofConstruction,
        etaCalculation,
        consensusValidationState,
        kesProductResource,
        ed25519Resource,
        bigBangHeader.slotId,
        operationalPeriodLength = ProtocolConfiguration.OperationalPeriodLength,
        activationOperationalPeriod = 0L,
        initializer.stakingAddress,
        initialSlot = 0L
      )
      staking = Staking.Eval.make(
        initializer.stakingAddress,
        LeaderElectionMinting.Eval.make(
          initializer.vrfVK,
          leaderElectionThreshold,
          vrfProofConstruction,
          statsInterpreter = StatsInterpreter.Noop.make[F],
          statsName = ""
        ),
        operationalKeys,
        consensusValidationState,
        etaCalculation,
        ed25519Resource,
        vrfProofConstruction,
        clock
      )
    } yield staking

}

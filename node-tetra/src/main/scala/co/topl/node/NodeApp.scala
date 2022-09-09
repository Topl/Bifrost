package co.topl.node

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.stream.scaladsl.Source
import akka.util.Timeout
import cats.effect.IO
import co.topl.catsakka._
import cats.implicits._
import co.topl.algebras._
import ClockAlgebra.implicits._
import cats.Applicative
import co.topl.blockchain._
import co.topl.catsakka.IOAkkaApp
import co.topl.crypto.hash.Blake2b512
import co.topl.crypto.signing._
import co.topl.interpreters._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.models._
import co.topl.typeclasses.implicits._
import co.topl.codecs.bytes.tetra.instances._
import com.typesafe.config.ConfigFactory
import co.topl.consensus._
import co.topl.consensus.algebras._
import co.topl.consensus.interpreters._
import co.topl.eventtree.ParentChildTree
import co.topl.ledger.interpreters._
import co.topl.minting._
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

        cryptoResources <- CryptoResources.make[F]

        dataStores <- DataStores.init[F](dataDir)(bigBangBlock)
        currentEventIdGetterSetters = new CurrentEventIdGetterSetters(dataStores.currentEventIds)

        canonicalHeadId       <- currentEventIdGetterSetters.canonicalHead.get()
        canonicalHeadSlotData <- dataStores.slotData.getOrRaise(canonicalHeadId)

        blockIdTree <- ParentChildTree.FromStore
          .make[F, TypedIdentifier](dataStores.parentChildTree, bigBangBlock.headerV2.parentHeaderId)
          .flatTap(_.associate(bigBangBlock.headerV2.id, bigBangBlock.headerV2.parentHeaderId))

        // Start the supporting interpreters
        blockHeightTree <- BlockHeightTree
          .make[F](
            dataStores.blockHeightTree,
            currentEventIdGetterSetters.blockHeightTree.get(),
            dataStores.slotData,
            blockIdTree,
            currentEventIdGetterSetters.blockHeightTree.set
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
          cryptoResources.blake2b256,
          cryptoResources.blake2b512
        )
        leaderElectionThreshold <- makeLeaderElectionThreshold(cryptoResources.blake2b512)
        consensusValidationState <- makeConsensusValidationState(
          clock,
          dataStores,
          currentEventIdGetterSetters,
          bigBangBlock,
          blockIdTree
        )
        localChain <- LocalChain.Eval.make(
          canonicalHeadSlotData,
          ChainSelection
            .orderT[F](
              dataStores.slotData.getOrRaise,
              cryptoResources.blake2b512,
              ProtocolConfiguration.ChainSelectionKLookback,
              ProtocolConfiguration.ChainSelectionSWindow
            )
        )
        mempool <- Mempool.make[F](
          currentEventIdGetterSetters.mempool.get(),
          dataStores.bodies.getOrRaise,
          dataStores.transactions.getOrRaise,
          blockIdTree,
          currentEventIdGetterSetters.mempool.set,
          clock,
          id => Logger[F].info(show"Expiring transaction id=$id"),
          1000L,
          1000L
        )
        implicit0(networkRandom: Random) = new Random(new SecureRandom())
        staking <- makeStaking(
          stakingDir,
          canonicalHeadSlotData,
          stakerInitializers.headOption.get,
          clock,
          etaCalculation,
          consensusValidationState,
          leaderElectionThreshold,
          cryptoResources.ed25519,
          cryptoResources.ed25519VRF,
          cryptoResources.kesProduct
        )
        validators <- Validators.make[F](
          cryptoResources,
          dataStores,
          currentEventIdGetterSetters,
          blockIdTree,
          etaCalculation,
          consensusValidationState,
          leaderElectionThreshold
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
            validators.header,
            validators.transactionSyntax,
            validators.bodySyntax,
            validators.bodySemantics,
            validators.bodyAuthorization,
            mempool,
            cryptoResources.ed25519VRF,
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
    currentHead:              SlotData,
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
      currentSlot  <- clock.globalSlot.map(_.max(0L))
      currentEpoch <- clock.epochOf(currentSlot)
      _            <- vrfProofConstruction.precomputeForEpoch(currentEpoch, currentHead.eta)
      operationalKeys <- OperationalKeys.FromSecureStore.make[F](
        secureStore = secureStore,
        clock = clock,
        vrfProof = vrfProofConstruction,
        etaCalculation,
        consensusValidationState,
        kesProductResource,
        ed25519Resource,
        currentHead.slotId,
        operationalPeriodLength = ProtocolConfiguration.OperationalPeriodLength,
        activationOperationalPeriod = 0L, // TODO: Accept registration block as `make` parameter?
        initializer.stakingAddress,
        initialSlot = currentSlot
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

  private def makeConsensusValidationState(
    clock:                       ClockAlgebra[F],
    dataStores:                  DataStores[F],
    currentEventIdGetterSetters: CurrentEventIdGetterSetters[F],
    bigBangBlock:                BlockV2.Full,
    blockIdTree:                 ParentChildTree[F, TypedIdentifier]
  ) =
    for {
      epochBoundariesState <- EpochBoundariesEventSourcedState.make[F](
        clock,
        currentEventIdGetterSetters.epochBoundaries.get(),
        blockIdTree,
        currentEventIdGetterSetters.epochBoundaries.set,
        dataStores.epochBoundaries.pure[F],
        dataStores.slotData.getOrRaise
      )
      consensusDataState <- ConsensusDataEventSourcedState.make[F](
        currentEventIdGetterSetters.consensusData.get(),
        blockIdTree,
        currentEventIdGetterSetters.consensusData.set,
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
    } yield consensusValidationState

  private def makeLeaderElectionThreshold(blake2b512Resource: UnsafeResource[F, Blake2b512]) =
    for {
      exp   <- ExpInterpreter.make[F](10000, 38)
      log1p <- Log1pInterpreter.make[F](10000, 8).flatMap(Log1pInterpreter.makeCached[F])
      leaderElectionThreshold = LeaderElectionValidation.Eval
        .make[F](ProtocolConfiguration.vrfConfig, blake2b512Resource, exp, log1p)
    } yield leaderElectionThreshold
}

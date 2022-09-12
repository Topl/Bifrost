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
    for {
      _ <- Logger[F].info(show"Launching node with args=$args")
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

      dataStores <- DataStores.init[F](bigBangBlock)

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
        cryptoResources.blake2b256,
        cryptoResources.blake2b512
      )
      leaderElectionThreshold <- makeLeaderElectionThreshold(cryptoResources.blake2b512)
      consensusValidationState <- makeConsensusValidationState(
        clock,
        dataStores,
        bigBangBlock,
        blockIdTree
      )
      localChain <- LocalChain.Eval.make(
        bigBangBlock.headerV2.slotData(Ed25519VRF.precomputed()),
        ChainSelection
          .orderT[F](
            dataStores.slotData.getOrRaise,
            cryptoResources.blake2b512,
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
      staking <- makeStaking(
        bigBangBlock.headerV2,
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
        bigBangBlock.headerV2,
        cryptoResources,
        dataStores,
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

  private def makeStaking(
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
      secureStore <- InMemorySecureStore.Eval.make[F]
      _           <- secureStore.write(UUID.randomUUID().toString, initializer.kesSK)
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

  private def makeConsensusValidationState(
    clock:        ClockAlgebra[F],
    dataStores:   DataStores[F],
    bigBangBlock: BlockV2.Full,
    blockIdTree:  ParentChildTree[F, TypedIdentifier]
  ) =
    for {
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
    } yield consensusValidationState

  private def makeLeaderElectionThreshold(blake2b512Resource: UnsafeResource[F, Blake2b512]) =
    for {
      exp   <- ExpInterpreter.make[F](10000, 38)
      log1p <- Log1pInterpreter.make[F](10000, 8).flatMap(Log1pInterpreter.makeCached[F])
      leaderElectionThreshold = LeaderElectionValidation.Eval
        .make[F](ProtocolConfiguration.vrfConfig, blake2b512Resource, exp, log1p)
    } yield leaderElectionThreshold
}

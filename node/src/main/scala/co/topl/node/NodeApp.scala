package co.topl.node

import cats.Applicative
import cats.effect.implicits._
import cats.effect.std.Random
import cats.effect.std.SecureRandom
import cats.effect.{IO, Resource, Sync}
import cats.implicits._
import co.topl.algebras._
import co.topl.blockchain._
import co.topl.blockchain.interpreters.{EpochDataEventSourcedState, EpochDataInterpreter}
import co.topl.codecs.bytes.tetra.instances._
import co.topl.common.application.IOBaseApp
import co.topl.consensus.algebras._
import co.topl.consensus.interpreters.ConsensusDataEventSourcedState.ConsensusData
import co.topl.consensus.interpreters.EpochBoundariesEventSourcedState.EpochBoundaries
import co.topl.consensus.models.VrfConfig
import co.topl.consensus.interpreters._
import co.topl.consensus.models.BlockId
import co.topl.crypto.hash.Blake2b256
import co.topl.crypto.hash.Blake2b512
import co.topl.crypto.signing._
import co.topl.eventtree.{EventSourcedState, ParentChildTree}
import co.topl.genus._
import co.topl.interpreters._
import co.topl.ledger.interpreters._
import co.topl.minting.algebras.StakingAlgebra
import co.topl.minting.interpreters.{OperationalKeyMaker, Staking, VrfCalculator}
import co.topl.models._
import co.topl.models.utility.HasLength.instances.byteStringLength
import co.topl.models.utility._
import co.topl.networking.p2p.{DisconnectedPeer, LocalPeer, RemoteAddress}
import co.topl.numerics.interpreters.{ExpInterpreter, Log1pInterpreter}
import co.topl.typeclasses.implicits._

import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}
// Hide `io` from fs2 because it conflicts with `io.grpc` down below
import fs2.{io => _, _}
import fs2.io.file.{Files, Path}
import kamon.Kamon
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import io.grpc.ServerServiceDefinition

import java.time.Instant
import java.util.UUID

object NodeApp extends AbstractNodeApp

abstract class AbstractNodeApp
    extends IOBaseApp[Args, ApplicationConfig](
      createArgs = args => Args.parserArgs.constructOrThrow(args),
      createConfig = IOBaseApp.createTypesafeConfig,
      parseConfig = (args, conf) => ApplicationConfig.unsafe(args, conf),
      preInitFunction = config => if (config.kamon.enable) Kamon.init()
    ) {
  def run: IO[Unit] = new ConfiguredNodeApp(args, appConfig).run
}

class ConfiguredNodeApp(args: Args, appConfig: ApplicationConfig) {

  type F[A] = IO[A]

  implicit private val logger: Logger[F] =
    Slf4jLogger.getLoggerFromName[F]("Bifrost.Node")

  def run: IO[Unit] = applicationResource.use_

  private def applicationResource: Resource[F, Unit] =
    for {
      _ <- Resource.pure[F, Unit](LoggingUtils.initialize(args))
      _ <- Resource.eval(Logger[F].info(show"Launching node with args=$args"))
      _ <- Resource.eval(Logger[F].info(show"Node configuration=$appConfig"))
      localPeer = LocalPeer(
        RemoteAddress(appConfig.bifrost.p2p.bindHost, appConfig.bifrost.p2p.bindPort),
        (0, 0)
      )
      implicit0(networkPrefix: NetworkPrefix) = NetworkPrefix(1: Byte)
      privateBigBang = appConfig.bifrost.bigBang.asInstanceOf[ApplicationConfig.Bifrost.BigBangs.Private]
      stakerInitializers <- Sync[F]
        .delay(
          PrivateTestnet.stakerInitializers(
            privateBigBang.timestamp,
            privateBigBang.stakerCount
          )
        )
        .toResource
      implicit0(bigBangConfig: BigBang.Config) <- Sync[F]
        .delay(
          PrivateTestnet.config(
            privateBigBang.timestamp,
            stakerInitializers,
            privateBigBang.stakes
          )
        )
        .toResource
      bigBangBlock = BigBang.block
      bigBangBlockId = bigBangBlock.header.id
      _ <- Resource.eval(Logger[F].info(show"Big Bang Block id=$bigBangBlockId"))

      stakingDir = Path(appConfig.bifrost.staking.directory) / bigBangBlockId.show
      _ <- Resource.eval(Files[F].createDirectories(stakingDir))
      _ <- Resource.eval(Logger[F].info(show"Using stakingDir=$stakingDir"))

      cryptoResources <- Resource.eval(CryptoResources.make[F])

      dataStores <- DataStoresInit.init[F](appConfig)(bigBangBlock)
      currentEventIdGetterSetters = new CurrentEventIdGetterSetters[F](dataStores.currentEventIds)

      canonicalHeadId       <- Resource.eval(currentEventIdGetterSetters.canonicalHead.get())
      canonicalHeadSlotData <- Resource.eval(dataStores.slotData.getOrRaise(canonicalHeadId))
      canonicalHead         <- Resource.eval(dataStores.headers.getOrRaise(canonicalHeadId))
      _                     <- Resource.eval(Logger[F].info(show"Canonical head id=$canonicalHeadId"))

      blockIdTree <- Resource.eval(
        ParentChildTree.FromReadWrite
          .make[F, BlockId](
            dataStores.parentChildTree.get,
            dataStores.parentChildTree.put,
            bigBangBlock.header.parentHeaderId
          )
          .flatTap(_.associate(bigBangBlockId, bigBangBlock.header.parentHeaderId))
      )

      // Start the supporting interpreters
      blockHeightTree <- Resource.eval(
        BlockHeightTree
          .make[F](
            dataStores.blockHeightTree,
            currentEventIdGetterSetters.blockHeightTree.get(),
            dataStores.slotData,
            blockIdTree,
            currentEventIdGetterSetters.blockHeightTree.set
          )
      )
      bigBangProtocol = appConfig.bifrost.protocols(0)
      vrfConfig = VrfConfig(
        bigBangProtocol.vrfLddCutoff,
        bigBangProtocol.vrfPrecision,
        bigBangProtocol.vrfBaselineDifficulty,
        bigBangProtocol.vrfAmplitude
      )
      ntpClockSkewer <- NtpClockSkewer
        .make[F](appConfig.bifrost.ntp.server, appConfig.bifrost.ntp.refreshInterval, appConfig.bifrost.ntp.timeout)
      clock <- SchedulerClock.make[F](
        bigBangProtocol.slotDuration,
        bigBangProtocol.epochLength,
        bigBangProtocol.operationalPeriodLength,
        Instant.ofEpochMilli(bigBangBlock.header.timestamp),
        bigBangProtocol.forwardBiasedSlotWindow,
        ntpClockSkewer
      )
      _ <- Resource.eval(
        clock.globalSlot.flatMap(globalSlot =>
          Logger[F].info(show"globalSlot=$globalSlot canonicalHeadSlot=${canonicalHeadSlotData.slotId.slot}")
        )
      )
      etaCalculation <- Resource.eval(
        EtaCalculation.make(
          dataStores.slotData.getOrRaise,
          clock,
          Sized.strictUnsafe(bigBangBlock.header.eligibilityCertificate.eta),
          cryptoResources.blake2b256,
          cryptoResources.blake2b512
        )
      )
      leaderElectionThreshold <- Resource.eval(makeLeaderElectionThreshold(cryptoResources.blake2b512, vrfConfig))

      epochBoundariesState <- Resource.eval(
        makeEpochBoundariesState(
          clock,
          dataStores,
          currentEventIdGetterSetters,
          blockIdTree
        )
      )
      consensusDataState <- Resource.eval(
        makeConsensusDataState(
          dataStores,
          currentEventIdGetterSetters,
          blockIdTree
        )
      )

      consensusValidationState <- Resource.eval(
        makeConsensusValidationState(
          epochBoundariesState,
          consensusDataState,
          clock,
          bigBangBlockId
        )
      )
      chainSelectionAlgebra = ChainSelection
        .make[F](
          dataStores.slotData.getOrRaise,
          cryptoResources.blake2b512,
          bigBangProtocol.chainSelectionKLookback,
          bigBangProtocol.chainSelectionSWindow
        )
      localChain <- Resource.eval(
        LocalChain.make(
          canonicalHeadSlotData,
          chainSelectionAlgebra,
          currentEventIdGetterSetters.canonicalHead.set
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
        appConfig.bifrost.mempool.defaultExpirationSlots
      )
      staking <- privateBigBang.localStakerIndex
        .flatMap(stakerInitializers.get(_))
        .fold(Resource.pure[F, Option[StakingAlgebra[F]]](none))(initializer =>
          makeStaking(
            stakingDir,
            initializer,
            clock,
            etaCalculation,
            consensusValidationState,
            leaderElectionThreshold,
            cryptoResources.ed25519,
            cryptoResources.blake2b256,
            cryptoResources.ed25519VRF,
            cryptoResources.kesProduct,
            bigBangProtocol,
            vrfConfig
          ).map(_.some)
        )

      eligibilityCache <-
        EligibilityCache
          .make[F](appConfig.bifrost.cache.eligibilities.maximumEntries.toInt)
          .evalTap(
            EligibilityCache.repopulate(
              _,
              appConfig.bifrost.cache.eligibilities.maximumEntries.toInt,
              canonicalHead,
              dataStores.headers.getOrRaise
            )
          )
      validators <- Validators.make[F](
        cryptoResources,
        dataStores,
        bigBangBlockId,
        eligibilityCache,
        currentEventIdGetterSetters,
        blockIdTree,
        etaCalculation,
        consensusValidationState,
        leaderElectionThreshold,
        clock
      )
      genusGrpcServices <-
        if (appConfig.genus.enable) {
          (
            for {
              genus <- Genus
                .make[F](
                  appConfig.bifrost.rpc.bindHost,
                  appConfig.bifrost.rpc.bindPort,
                  Some(appConfig.genus.orientDbDirectory)
                    .filterNot(_.isEmpty)
                    .getOrElse(dataStores.baseDirectory./("orient-db").toString),
                  appConfig.genus.orientDbPassword
                )
              _ <- Replicator.background(genus)
              definitions <- GenusGrpc.Server.services(
                genus.blockFetcher,
                genus.transactionFetcher,
                genus.vertexFetcher
              )
            } yield definitions
          )
            .recoverWith { case e =>
              Logger[F]
                .warn(e)("Failed to start Genus server, continuing without it")
                .void
                .as(Nil)
                .toResource
            }
        } else
          Resource.pure[F, List[ServerServiceDefinition]](Nil)
      implicit0(random: Random[F]) <- SecureRandom.javaSecuritySecureRandom[F].toResource

      protocolConfig <- ProtocolConfiguration.make[F](
        appConfig.bifrost.protocols.map { case (slot, protocol) => protocol.nodeConfig(slot) }.toSeq
      )

      transactionRewardCalculator <- TransactionRewardCalculator.make[F]

      epochDataEventSourcedState <- EpochDataEventSourcedState.make[F](
        currentEventIdGetterSetters.epochData.get(),
        bigBangBlockId,
        blockIdTree,
        currentEventIdGetterSetters.epochData.set,
        dataStores.epochData.pure[F],
        clock,
        dataStores.headers.getOrRaise,
        dataStores.bodies.getOrRaise,
        dataStores.transactions.getOrRaise,
        transactionRewardCalculator,
        epochBoundariesState,
        consensusDataState
      )

      epochData <- EpochDataInterpreter
        .make[F](Sync[F].defer(localChain.head).map(_.slotId.blockId), epochDataEventSourcedState)

      // Finally, run the program
      _ <- Blockchain
        .make[F](
          clock,
          staking,
          dataStores,
          localChain,
          chainSelectionAlgebra,
          blockIdTree,
          blockHeightTree,
          validators,
          mempool,
          cryptoResources.ed25519VRF,
          localPeer,
          Stream.eval(clock.delayedUntilSlot(canonicalHeadSlotData.slotId.slot)) >>
          Stream.iterable[F, DisconnectedPeer](appConfig.bifrost.p2p.knownPeers) ++
          Stream.never[F],
          appConfig.bifrost.rpc.bindHost,
          appConfig.bifrost.rpc.bindPort,
          protocolConfig,
          genusGrpcServices,
          appConfig.bifrost.p2p.experimental.getOrElse(false),
          epochData,
          appConfig.bifrost.p2p.pingPongInterval.getOrElse(FiniteDuration(0, MILLISECONDS))
        )
    } yield ()

  private def makeStaking(
    stakingDir:               Path,
    initializer:              StakerInitializers.Operator,
    clock:                    ClockAlgebra[F],
    etaCalculation:           EtaCalculationAlgebra[F],
    consensusValidationState: ConsensusValidationStateAlgebra[F],
    leaderElectionThreshold:  LeaderElectionValidationAlgebra[F],
    ed25519Resource:          UnsafeResource[F, Ed25519],
    blake2b256Resource:       UnsafeResource[F, Blake2b256],
    ed25519VRFResource:       UnsafeResource[F, Ed25519VRF],
    kesProductResource:       UnsafeResource[F, KesProduct],
    protocol:                 ApplicationConfig.Bifrost.Protocol,
    vrfConfig:                VrfConfig
  ) =
    for {
      // Initialize a persistent secure store
      secureStore <- CatsSecureStore.make[F](stakingDir.toNioPath)
      // Determine if a key has already been initialized
      _ <- secureStore.list
        .map(_.isEmpty)
        // If uninitialized, generate a new key.  Otherwise, move on.
        .ifM(secureStore.write(UUID.randomUUID().toString, initializer.kesSK), Applicative[F].unit)
        .toResource

      vrfCalculator <- VrfCalculator.make[F](
        initializer.vrfSK,
        ed25519VRFResource,
        protocol.vrfCacheSize
      )

      operationalKeys <- OperationalKeyMaker
        .make[F](
          operationalPeriodLength = protocol.operationalPeriodLength,
          activationOperationalPeriod = 0L, // TODO: Accept registration block as `make` parameter?
          initializer.stakingAddress,
          vrfConfig,
          secureStore = secureStore,
          clock = clock,
          vrfCalculator = vrfCalculator,
          leaderElectionThreshold,
          etaCalculation,
          consensusValidationState,
          kesProductResource,
          ed25519Resource
        )

      staking <- Staking.make(
        initializer.stakingAddress,
        initializer.vrfVK,
        operationalKeys,
        consensusValidationState,
        etaCalculation,
        ed25519Resource,
        blake2b256Resource,
        vrfCalculator,
        leaderElectionThreshold
      )
    } yield staking

  private def makeEpochBoundariesState(
    clock:                       ClockAlgebra[F],
    dataStores:                  DataStores[F],
    currentEventIdGetterSetters: CurrentEventIdGetterSetters[F],
    blockIdTree:                 ParentChildTree[F, BlockId]
  ): F[EventSourcedState[F, EpochBoundaries[F], BlockId]] =
    for {
      epochBoundariesState <- EpochBoundariesEventSourcedState.make[F](
        clock,
        currentEventIdGetterSetters.epochBoundaries.get(),
        blockIdTree,
        currentEventIdGetterSetters.epochBoundaries.set,
        dataStores.epochBoundaries.pure[F],
        dataStores.slotData.getOrRaise
      )
    } yield epochBoundariesState

  private def makeConsensusDataState(
    dataStores:                  DataStores[F],
    currentEventIdGetterSetters: CurrentEventIdGetterSetters[F],
    blockIdTree:                 ParentChildTree[F, BlockId]
  ): F[EventSourcedState[F, ConsensusDataEventSourcedState.ConsensusData[F], BlockId]] =
    for {
      consensusDataState <- ConsensusDataEventSourcedState.make[F](
        currentEventIdGetterSetters.consensusData.get(),
        blockIdTree,
        currentEventIdGetterSetters.consensusData.set,
        ConsensusDataEventSourcedState
          .ConsensusData(
            dataStores.activeStake,
            dataStores.inactiveStake,
            dataStores.registrations
          )
          .pure[F],
        dataStores.bodies.getOrRaise,
        dataStores.transactions.getOrRaise
      )
    } yield consensusDataState

  private def makeConsensusValidationState(
    epochBoundariesState: EventSourcedState[F, EpochBoundaries[F], BlockId],
    consensusDataState:   EventSourcedState[F, ConsensusData[F], BlockId],
    clock:                ClockAlgebra[F],
    bigBangBlockId:       BlockId
  ): F[ConsensusValidationStateAlgebra[F]] =
    ConsensusValidationState
      .make[F](bigBangBlockId, epochBoundariesState, consensusDataState, clock)

  private def makeLeaderElectionThreshold(blake2b512Resource: UnsafeResource[F, Blake2b512], vrfConfig: VrfConfig) =
    for {
      exp   <- ExpInterpreter.make[F](10000, 38)
      log1p <- Log1pInterpreter.make[F](10000, 8).flatMap(Log1pInterpreter.makeCached[F])
      leaderElectionThreshold = LeaderElectionValidation
        .make[F](vrfConfig, blake2b512Resource, exp, log1p)
    } yield leaderElectionThreshold
}

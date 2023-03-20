package co.topl.node

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import cats.Applicative
import cats.effect.implicits._
import cats.effect.{IO, Resource, Sync}
import cats.implicits._
import co.topl.algebras._
import co.topl.blockchain._
import co.topl.catsakka._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.common.application.{IOAkkaApp, IOBaseApp}
import co.topl.consensus.algebras._
import co.topl.consensus.models.{SlotData, VrfConfig}
import co.topl.consensus.interpreters._
import co.topl.consensus.models.BlockId
import co.topl.crypto.hash.Blake2b256
import co.topl.crypto.hash.Blake2b512
import co.topl.crypto.signing._
import co.topl.eventtree.ParentChildTree
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
import fs2._
import fs2.io.file.{Files, Path}
import kamon.Kamon
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.security.SecureRandom
import java.time.Instant
import java.util.UUID
import scala.util.Random

object NodeApp extends AbstractNodeApp

abstract class AbstractNodeApp
    extends IOAkkaApp[Args, ApplicationConfig, Nothing](
      createArgs = args => Args.parserArgs.constructOrThrow(args),
      createConfig = IOBaseApp.createTypesafeConfig,
      parseConfig = (args, conf) => ApplicationConfig.unsafe(args, conf),
      createSystem = (_, _, conf) => ActorSystem[Nothing](Behaviors.empty, "Bifrost", conf),
      preInitFunction = config => if (config.kamon.enable) Kamon.init()
    ) {
  def run: IO[Unit] = new ConfiguredNodeApp(args, appConfig).run
}

class ConfiguredNodeApp(args: Args, appConfig: ApplicationConfig)(implicit system: ActorSystem[_]) {

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

      dataStores <- DataStores.init[F](appConfig)(bigBangBlock)
      currentEventIdGetterSetters = new CurrentEventIdGetterSetters(dataStores.currentEventIds)

      canonicalHeadId       <- Resource.eval(currentEventIdGetterSetters.canonicalHead.get())
      canonicalHeadSlotData <- Resource.eval(dataStores.slotData.getOrRaise(canonicalHeadId))
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
      consensusValidationState <- Resource.eval(
        makeConsensusValidationState(
          clock,
          dataStores,
          currentEventIdGetterSetters,
          bigBangBlockId,
          blockIdTree
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
        appConfig.bifrost.mempool.defaultExpirationSlots,
        appConfig.bifrost.mempool.duplicateSpenderExpirationSlots
      )
      implicit0(networkRandom: Random) = new Random(new SecureRandom())
      staking <- privateBigBang.localStakerIndex
        .flatMap(stakerInitializers.get(_))
        .fold(Resource.pure[F, Option[StakingAlgebra[F]]](none))(initializer =>
          makeStaking(
            stakingDir,
            canonicalHeadSlotData,
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
      validators <- Resource.eval(
        Validators.make[F](
          cryptoResources,
          dataStores,
          currentEventIdGetterSetters,
          blockIdTree,
          etaCalculation,
          consensusValidationState,
          leaderElectionThreshold,
          clock
        )
      )
      // Finally, run the program
      _ <- Blockchain
        .make[F](
          clock,
          staking,
          dataStores.slotData,
          dataStores.headers,
          dataStores.bodies,
          dataStores.transactions,
          localChain,
          chainSelectionAlgebra,
          blockIdTree,
          blockHeightTree,
          validators.header,
          validators.headerToBody,
          validators.transactionSyntax,
          validators.bodySyntax,
          validators.bodySemantics,
          validators.bodyAuthorization,
          mempool,
          cryptoResources.ed25519VRF,
          localPeer,
          Stream.eval(clock.delayedUntilSlot(canonicalHeadSlotData.slotId.slot)) >>
          Stream.iterable[F, DisconnectedPeer](appConfig.bifrost.p2p.knownPeers) ++
          Stream.never[F],
          appConfig.bifrost.rpc.bindHost,
          appConfig.bifrost.rpc.bindPort,
          appConfig.bifrost.p2p.experimental.getOrElse(false)
        )
    } yield ()

  private def makeStaking(
    stakingDir:               Path,
    currentHead:              SlotData,
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
        clock,
        leaderElectionThreshold,
        ed25519VRFResource,
        vrfConfig,
        protocol.vrfCacheSize
      )

      operationalKeys <- OperationalKeyMaker
        .make[F](
          currentHead.slotId,
          operationalPeriodLength = protocol.operationalPeriodLength,
          activationOperationalPeriod = 0L, // TODO: Accept registration block as `make` parameter?
          initializer.stakingAddress,
          secureStore = secureStore,
          clock = clock,
          vrfCalculator = vrfCalculator,
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

  private def makeConsensusValidationState(
    clock:                       ClockAlgebra[F],
    dataStores:                  DataStores[F],
    currentEventIdGetterSetters: CurrentEventIdGetterSetters[F],
    bigBangBlockId:              BlockId,
    blockIdTree:                 ParentChildTree[F, BlockId]
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
        dataStores.transactions.getOrRaise
      )
      consensusValidationState <- ConsensusValidationState
        .make[F](bigBangBlockId, epochBoundariesState, consensusDataState, clock)
    } yield consensusValidationState

  private def makeLeaderElectionThreshold(blake2b512Resource: UnsafeResource[F, Blake2b512], vrfConfig: VrfConfig) =
    for {
      exp   <- ExpInterpreter.make[F](10000, 38)
      log1p <- Log1pInterpreter.make[F](10000, 8).flatMap(Log1pInterpreter.makeCached[F])
      leaderElectionThreshold = LeaderElectionValidation
        .make[F](vrfConfig, blake2b512Resource, exp, log1p)
    } yield leaderElectionThreshold
}

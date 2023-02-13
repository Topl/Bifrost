package co.topl.node

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import cats.Applicative
import cats.effect.{IO, Resource}
import cats.implicits._
import co.topl.algebras._
import co.topl.blockchain._
import co.topl.catsakka._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.common.application.{IOAkkaApp, IOBaseApp}
import co.topl.consensus.algebras._
import co.topl.consensus.models.VrfConfig
import co.topl.consensus.models.SlotData
import co.topl.consensus.interpreters._
import co.topl.crypto.hash.Blake2b512
import co.topl.crypto.signing._
import co.topl.eventtree.ParentChildTree
import co.topl.interpreters._
import co.topl.ledger.interpreters._
import co.topl.minting.algebras.StakingAlgebra
import co.topl.minting.interpreters.{OperationalKeyMaker, Staking, VrfCalculator}
import co.topl.models._
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

object NodeApp
    extends IOAkkaApp[Args, ApplicationConfig, Nothing](
      createArgs = args => Args.parserArgs.constructOrThrow(args),
      createConfig = IOBaseApp.createTypesafeConfig,
      parseConfig = (args, conf) => ApplicationConfig.unsafe(args, conf),
      createSystem = (_, _, conf) => ActorSystem[Nothing](Behaviors.empty, "Bifrost", conf),
      preInitFunction = config => if (config.kamon.enable) Kamon.init()
    ) {

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
      stakerInitializers = PrivateTestnet.stakerInitializers(
        privateBigBang.timestamp,
        privateBigBang.stakerCount
      )
      implicit0(bigBangConfig: BigBang.Config) = PrivateTestnet.config(
        privateBigBang.timestamp,
        stakerInitializers
      )
      bigBangBlock = BigBang.block
      _ <- Resource.eval(Logger[F].info(show"Big Bang Block id=${bigBangBlock.header.id.asTypedBytes}"))

      stakingDir = Path(appConfig.bifrost.staking.directory) / bigBangBlock.header.id.asTypedBytes.show
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
          .make[F, TypedIdentifier](
            dataStores.parentChildTree.get,
            dataStores.parentChildTree.put,
            bigBangBlock.header.parentHeaderId
          )
          .flatTap(_.associate(bigBangBlock.header.id, bigBangBlock.header.parentHeaderId))
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
      clock = SchedulerClock.Eval.make[F](
        bigBangProtocol.slotDuration,
        bigBangProtocol.epochLength,
        Instant.ofEpochMilli(bigBangBlock.header.timestamp),
        bigBangProtocol.forwardBiasedSlotWindow
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
          bigBangBlock.header.eligibilityCertificate.eta,
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
          bigBangBlock,
          blockIdTree
        )
      )
      localChain <- Resource.eval(
        LocalChain.make(
          canonicalHeadSlotData,
          ChainSelection
            .make[F](
              dataStores.slotData.getOrRaise,
              cryptoResources.blake2b512,
              bigBangProtocol.chainSelectionKLookback,
              bigBangProtocol.chainSelectionSWindow
            ),
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
          appConfig.bifrost.rpc.bindPort
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
    ed25519VRFResource:       UnsafeResource[F, Ed25519VRF],
    kesProductResource:       UnsafeResource[F, KesProduct],
    protocol:                 ApplicationConfig.Bifrost.Protocol,
    vrfConfig:                VrfConfig
  ) =
    // Initialize a persistent secure store
    CatsSecureStore
      .make[F](stakingDir.toNioPath)
      .evalMap(secureStore =>
        for {
          // Determine if a key has already been initialized
          _ <- secureStore.list
            .map(_.isEmpty)
            // If uninitialized, generate a new key.  Otherwise, move on.
            .ifM(secureStore.write(UUID.randomUUID().toString, initializer.kesSK), Applicative[F].unit)
          vrfCalculator <- VrfCalculator.make[F](
            initializer.vrfVK,
            initializer.vrfSK,
            clock,
            leaderElectionThreshold,
            ed25519VRFResource,
            vrfConfig,
            protocol.vrfCacheSize
          )
          currentSlot <- clock.globalSlot.map(_.max(0L))

          operationalKeys <- OperationalKeyMaker.make[F](
            initialSlot = currentSlot,
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
          staking = Staking.make(
            initializer.stakingAddress,
            operationalKeys,
            consensusValidationState,
            etaCalculation,
            ed25519Resource,
            vrfCalculator
          )
        } yield staking
      )

  private def makeConsensusValidationState(
    clock:                       ClockAlgebra[F],
    dataStores:                  DataStores[F],
    currentEventIdGetterSetters: CurrentEventIdGetterSetters[F],
    bigBangBlock:                Block.Full,
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
        .make[F](bigBangBlock.header.id, epochBoundariesState, consensusDataState, clock)
    } yield consensusValidationState

  private def makeLeaderElectionThreshold(blake2b512Resource: UnsafeResource[F, Blake2b512], vrfConfig: VrfConfig) =
    for {
      exp   <- ExpInterpreter.make[F](10000, 38)
      log1p <- Log1pInterpreter.make[F](10000, 8).flatMap(Log1pInterpreter.makeCached[F])
      leaderElectionThreshold = LeaderElectionValidation
        .make[F](vrfConfig, blake2b512Resource, exp, log1p)
    } yield leaderElectionThreshold
}

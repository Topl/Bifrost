package co.topl.node

import cats.data.OptionT
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
import co.topl.config.ApplicationConfig
import co.topl.consensus.interpreters.EpochBoundariesEventSourcedState.EpochBoundaries
import co.topl.consensus.models.{BlockId, ProtocolVersion, VrfConfig}
import co.topl.consensus.interpreters._
import co.topl.crypto.hash.Blake2b512
import co.topl.eventtree.{EventSourcedState, ParentChildTree}
import co.topl.genus._
import co.topl.interpreters._
import co.topl.ledger.interpreters._
import co.topl.models.utility.HasLength.instances.byteStringLength
import co.topl.models.utility._
import co.topl.networking.p2p.{LocalPeer, RemoteAddress}
import co.topl.numerics.interpreters.{ExpInterpreter, Log1pInterpreter}
import co.topl.typeclasses.implicits._
import co.topl.node.ApplicationConfigOps._
import co.topl.node.cli.ConfiguredCliApp
import co.topl.node.models.FullBlock
import co.topl.numerics.implicits._
import fs2.io.file.{Files, Path}
import kamon.Kamon
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import io.grpc.ServerServiceDefinition

import java.time.Instant

object NodeApp extends AbstractNodeApp

abstract class AbstractNodeApp
    extends IOBaseApp[Args, ApplicationConfig](
      createArgs = Args.parse,
      createConfig = IOBaseApp.createTypesafeConfig,
      parseConfig = (args, conf) => ApplicationConfigOps.unsafe(args, conf),
      preInitFunction = config => if (config.kamon.enable) Kamon.init()
    ) {

  def run: IO[Unit] =
    if (args.startup.cli) new ConfiguredCliApp(appConfig).run
    else new ConfiguredNodeApp(args, appConfig).run
}

class ConfiguredNodeApp(args: Args, appConfig: ApplicationConfig) {

  type F[A] = IO[A]

  implicit private val logger: Logger[F] =
    Slf4jLogger.getLoggerFromName[F]("Bifrost.Node")

  def run: IO[Unit] = applicationResource.use_

  private def applicationResource: Resource[F, Unit] =
    for {
      _ <- Sync[F].delay(LoggingUtils.initialize(args)).toResource
      _ <- Logger[F].info(show"Launching node with args=$args").toResource
      _ <- Logger[F].info(show"Node configuration=$appConfig").toResource
      localPeer = LocalPeer(
        RemoteAddress(appConfig.bifrost.p2p.bindHost, appConfig.bifrost.p2p.bindPort),
        (0, 0)
      )
      protocolVersion <- Sync[F]
        .delay(ProtocolVersioner.apply(appConfig.bifrost.protocols).appVersion.asProtocolVersion)
        .toResource
      bigBangBlock <- initializeBigBang(protocolVersion)
      bigBangBlockId = bigBangBlock.header.id
      _ <- Logger[F].info(show"Big Bang Block id=$bigBangBlockId").toResource

      stakingDir = Path(interpolateBlockId(bigBangBlockId)(appConfig.bifrost.staking.directory))
      _ <- Files[F].createDirectories(stakingDir).toResource
      _ <- Logger[F].info(show"Using stakingDir=$stakingDir").toResource

      cryptoResources <- CryptoResources.make[F].toResource

      dataStores <- DataStoresInit.init[F](appConfig)(bigBangBlock)
      currentEventIdGetterSetters = new CurrentEventIdGetterSetters[F](dataStores.currentEventIds)

      canonicalHeadId       <- currentEventIdGetterSetters.canonicalHead.get().toResource
      canonicalHeadSlotData <- dataStores.slotData.getOrRaise(canonicalHeadId).toResource
      canonicalHead         <- dataStores.headers.getOrRaise(canonicalHeadId).toResource
      _                     <- Logger[F].info(show"Canonical head id=$canonicalHeadId").toResource

      blockIdTree <- ParentChildTree.FromReadWrite
        .make[F, BlockId](
          dataStores.parentChildTree.get,
          dataStores.parentChildTree.put,
          bigBangBlock.header.parentHeaderId
        )
        .flatTap(_.associate(bigBangBlockId, bigBangBlock.header.parentHeaderId))
        .toResource

      // Start the supporting interpreters
      blockHeightTree <- BlockHeightTree
        .make[F](
          dataStores.blockHeightTree,
          currentEventIdGetterSetters.blockHeightTree.get(),
          dataStores.slotData,
          blockIdTree,
          currentEventIdGetterSetters.blockHeightTree.set
        )
        .toResource
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
      globalSlot <- clock.globalSlot.toResource
      _ <- Logger[F]
        .info(show"globalSlot=$globalSlot canonicalHeadSlot=${canonicalHeadSlotData.slotId.slot}")
        .toResource
      etaCalculation <-
        EtaCalculation
          .make(
            dataStores.slotData.getOrRaise,
            clock,
            Sized.strictUnsafe(bigBangBlock.header.eligibilityCertificate.eta),
            cryptoResources.blake2b256,
            cryptoResources.blake2b512
          )
          .toResource
      leaderElectionThreshold <- makeLeaderElectionThreshold(cryptoResources.blake2b512, vrfConfig).toResource

      epochBoundariesState <- makeEpochBoundariesState(
        clock,
        dataStores,
        currentEventIdGetterSetters,
        blockIdTree
      ).toResource
      consensusDataState <-
        makeConsensusDataState(
          dataStores,
          currentEventIdGetterSetters,
          blockIdTree
        ).toResource

      consensusValidationState <- ConsensusValidationState
        .make[F](bigBangBlockId, epochBoundariesState, consensusDataState, clock)
        .toResource
      chainSelectionAlgebra = ChainSelection
        .make[F](
          dataStores.slotData.getOrRaise,
          cryptoResources.blake2b512,
          bigBangProtocol.chainSelectionKLookback,
          bigBangProtocol.chainSelectionSWindow
        )
      localChain <-
        LocalChain
          .make(
            canonicalHeadSlotData,
            chainSelectionAlgebra,
            currentEventIdGetterSetters.canonicalHead.set
          )
          .toResource
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
      staking <-
        OptionT
          .liftF(StakingInit.stakingIsInitialized[F](stakingDir).toResource)
          .flatMap(
            OptionT.whenF(_)(
              StakingInit
                .makeStakingFromDisk(
                  stakingDir,
                  appConfig.bifrost.staking.rewardAddress,
                  clock,
                  etaCalculation,
                  consensusValidationState,
                  leaderElectionThreshold,
                  cryptoResources,
                  bigBangProtocol,
                  vrfConfig,
                  protocolVersion
                )
            )
          )
          .value

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
          appConfig.bifrost.p2p.knownPeers,
          appConfig.bifrost.rpc.bindHost,
          appConfig.bifrost.rpc.bindPort,
          protocolConfig,
          genusGrpcServices,
          epochData,
          appConfig.bifrost.p2p.networkProperties
        )
    } yield ()

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
          .ConsensusData(dataStores.activeStake, dataStores.inactiveStake, dataStores.registrations)
          .pure[F],
        dataStores.bodies.getOrRaise,
        dataStores.transactions.getOrRaise
      )
    } yield consensusDataState

  private def makeLeaderElectionThreshold(blake2b512Resource: UnsafeResource[F, Blake2b512], vrfConfig: VrfConfig) =
    for {
      exp   <- ExpInterpreter.make[F](10000, 38)
      log1p <- Log1pInterpreter.make[F](10000, 8).flatMap(Log1pInterpreter.makeCached[F])
      leaderElectionThreshold = LeaderElectionValidation
        .make[F](vrfConfig, blake2b512Resource, exp, log1p)
    } yield leaderElectionThreshold

  /**
   * Based on the application config, determines if the genesis block is a public network or a private testnet, and
   * initialize it accordingly
   */
  private def initializeBigBang(protocolVersion: ProtocolVersion): Resource[F, FullBlock] =
    appConfig.bifrost.bigBang match {
      case privateBigBang: ApplicationConfig.Bifrost.BigBangs.Private =>
        for {
          testnetStakerInitializers <- Sync[F]
            .delay(PrivateTestnet.stakerInitializers(privateBigBang.timestamp, privateBigBang.stakerCount))
            .toResource
          bigBangConfig <- Sync[F]
            .delay(
              PrivateTestnet
                .config(privateBigBang.timestamp, testnetStakerInitializers, privateBigBang.stakes, protocolVersion)
            )
            .toResource
          bigBangBlock = BigBang.fromConfig(bigBangConfig)
          _ <- privateBigBang.localStakerIndex.traverse(index =>
            PrivateTestnet
              .writeStaker[F](
                Path(interpolateBlockId(bigBangBlock.header.id)(appConfig.bifrost.staking.directory)),
                testnetStakerInitializers(index),
                privateBigBang.stakes.fold(PrivateTestnet.defaultStake(privateBigBang.stakerCount))(_.apply(index))
              )
              .toResource
          )
        } yield bigBangBlock
      case publicBigBang: ApplicationConfig.Bifrost.BigBangs.Public =>
        for {
          reader               <- DataReaders.fromSourcePath[F](publicBigBang.sourcePath)
          headerBodyValidation <- BlockHeaderToBodyValidation.make[F]().toResource
          bigBangBlock         <- BigBang.fromRemote(reader)(headerBodyValidation)(publicBigBang.genesisId).toResource
        } yield bigBangBlock
    }
}

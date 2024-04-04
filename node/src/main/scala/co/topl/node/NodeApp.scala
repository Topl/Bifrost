package co.topl.node

import cats.data.OptionT
import cats.effect.implicits._
import cats.effect.std.Random
import cats.effect.std.SecureRandom
import cats.effect.{Async, IO, Resource, Sync}
import cats.implicits._
import co.topl.blockchain._
import co.topl.blockchain.interpreters.{EpochDataEventSourcedState, EpochDataInterpreter, NodeMetadata}
import co.topl.brambl.validation.{TransactionCostCalculatorInterpreter, TransactionCostConfig}
import co.topl.buildinfo.node.BuildInfo
import co.topl.catsutils._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.common.application.IOBaseApp
import co.topl.config.ApplicationConfig
import co.topl.config.ApplicationConfig.Bifrost.KnownPeer
import co.topl.consensus.models.{BlockId, VrfConfig}
import co.topl.consensus.interpreters._
import co.topl.crypto.hash.Blake2b512
import co.topl.crypto.signing.Ed25519
import co.topl.eventtree.ParentChildTree
import co.topl.genus._
import co.topl.grpc.HealthCheckGrpc
import co.topl.healthcheck.HealthCheck
import co.topl.interpreters._
import co.topl.ledger.interpreters._
import co.topl.models.utility.HasLength.instances.byteStringLength
import co.topl.models.utility._
import co.topl.networking.p2p.{LocalPeer, RemoteAddress}
import co.topl.numerics.interpreters.{ExpInterpreter, Log1pInterpreter}
import co.topl.typeclasses.implicits._
import co.topl.node.ApplicationConfigOps._
import co.topl.node.cli.ConfiguredCliApp
import co.topl.version.VersionReplicator
import com.google.protobuf.ByteString
import com.typesafe.config.Config
import fs2.io.file.Path
import kamon.Kamon
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration._
import java.time.Instant

object NodeApp extends AbstractNodeApp

abstract class AbstractNodeApp
    extends IOBaseApp[Args, ApplicationConfig](
      createArgs = a => IO.delay(Args.parse(a)),
      createConfig = IOBaseApp.createTypesafeConfig(_, AbstractNodeApp.ConfigFileEnvironmentVariable.some),
      parseConfig = (args, conf) => IO.delay(ApplicationConfigOps.unsafe(args, conf)),
      preInitFunction = config => IO.delay(if (config.kamon.enable) Kamon.init())
    ) {

  def run(cmdArgs: Args, config: Config, appConfig: ApplicationConfig): IO[Unit] =
    if (cmdArgs.startup.cli) new ConfiguredCliApp(appConfig).run
    else if (cmdArgs.startup.idle) new IdleApp(appConfig).run
    else if (cmdArgs.startup.pruneDir.isDefined) new PrunedDataStoresApp(appConfig, cmdArgs.startup.pruneDir.get).run
    else new ConfiguredNodeApp(cmdArgs, appConfig).run
}

object AbstractNodeApp {
  final val ConfigFileEnvironmentVariable = "BIFROST_CONFIG_FILE"
}

class ConfiguredNodeApp(args: Args, appConfig: ApplicationConfig) {

  type F[A] = IO[A]

  def run: IO[Unit] = applicationResource.use_

  // scalastyle:off method.length
  private def applicationResource: Resource[F, Unit] =
    for {
      implicit0(syncF: Async[F])   <- Resource.pure(implicitly[Async[F]])
      implicit0(logger: Logger[F]) <- Resource.pure(Slf4jLogger.getLoggerFromName[F]("Bifrost.Node"))

      _ <- Sync[F].delay(LoggingUtils.initialize(args)).toResource
      _ <- Logger[F].info(show"Launching node with args=$args").toResource
      _ <- Logger[F].info(show"Node configuration=$appConfig").toResource

      cryptoResources            <- CryptoResources.make[F].toResource
      (bigBangBlock, dataStores) <- DataStoresInit.initializeData(appConfig)

      metadata <- NodeMetadata.make(dataStores.metadata)
      _        <- metadata.setAppVersion(BuildInfo.version).toResource
      _ <- OptionT(metadata.readInitTime)
        .flatTapNone(IO.realTime.map(_.toMillis).flatMap(metadata.setInitTime))
        .value
        .toResource

      implicit0(random: Random[F]) <- SecureRandom.javaSecuritySecureRandom[F].toResource

      p2pSK <- OptionT(metadata.readP2PSK)
        .getOrElseF(
          random
            .nextBytes(32)
            .flatMap(seed =>
              cryptoResources.ed25519
                .use(e => Sync[F].delay(e.deriveSecretKeyFromSeed(seed)))
                .map(_.bytes)
                .map(ByteString.copyFrom)
            )
            .flatTap(metadata.setP2PSK)
        )
        .toResource
      p2pVK <- cryptoResources.ed25519
        .use(e => Sync[F].delay(e.getVerificationKey(Ed25519.SecretKey(p2pSK.toByteArray))))
        .map(_.bytes)
        .map(ByteString.copyFrom)
        .toResource

      localPeer = LocalPeer(
        RemoteAddress(appConfig.bifrost.p2p.bindHost, appConfig.bifrost.p2p.bindPort),
        p2pVK,
        p2pSK
      )

      bigBangBlockId = bigBangBlock.header.id
      bigBangSlotData <- dataStores.slotData.getOrRaise(bigBangBlockId).toResource
      _ <- Logger[F].info(show"Big Bang Block id=$bigBangBlockId timestamp=${bigBangBlock.header.timestamp}").toResource

      stakingDir = Path(interpolateBlockId(bigBangBlockId)(appConfig.bifrost.staking.directory))
      _ <- Logger[F].info(show"Using stakingDir=$stakingDir").toResource

      currentEventIdGetterSetters = new CurrentEventIdGetterSetters[F](dataStores.currentEventIds)

      canonicalHeadId       <- currentEventIdGetterSetters.canonicalHead.get().toResource
      canonicalHeadSlotData <- dataStores.slotData.getOrRaise(canonicalHeadId).toResource
      canonicalHead         <- dataStores.headers.getOrRaise(canonicalHeadId).toResource
      _ <- Logger[F]
        .info(
          show"Canonical head" +
          show" id=$canonicalHeadId" +
          show" height=${canonicalHeadSlotData.height}" +
          show" slot=${canonicalHeadSlotData.slotId.slot}"
        )
        .toResource

      blockIdTree <- ParentChildTree.FromReadWrite
        .make[F, BlockId](
          dataStores.parentChildTree.get,
          dataStores.parentChildTree.put,
          bigBangBlock.header.parentHeaderId
        )
        .toResource

      // Start the supporting interpreters
      txIdToBlockIdTree <- TxIdToBlockIdTree
        .make(
          canonicalHeadId.pure[F],
          dataStores.bodies.get,
          dataStores.txIdToBlockId,
          blockIdTree
        )
        .toResource
      blockHeightTreeLocal <- BlockHeightTree
        .make[F](
          dataStores.blockHeightTreeLocal,
          currentEventIdGetterSetters.blockHeightTreeLocal.get(),
          dataStores.slotData,
          blockIdTree,
          currentEventIdGetterSetters.blockHeightTreeLocal.set
        )
        .toResource
      blockHeightTreeP2P <- BlockHeightTree
        .make[F](
          dataStores.blockHeightTreeP2P,
          currentEventIdGetterSetters.blockHeightTreeP2P.get(),
          dataStores.slotData,
          blockIdTree,
          currentEventIdGetterSetters.blockHeightTreeP2P.set
        )
        .toResource
      _ <- OptionT(blockHeightTreeLocal.useStateAt(canonicalHeadId)(_.apply(BigBang.Height)))
        .ensure(new IllegalStateException("The configured genesis block does not match the stored genesis block."))(
          _ === bigBangBlockId
        )
        .value
        .toResource
      bigBangProtocol <-
        BigBang
          .extractProtocol(bigBangBlock)
          .leftMap(error =>
            new IllegalArgumentException(s"Genesis block contained invalid protocol settings. reason=$error")
          )
          .pure[F]
          .rethrow
          .flatMap(protocol =>
            appConfig.bifrost
              .protocols(0)
              .epochLengthOverride
              .foldLeftM(
                protocol
              )((protocol, lengthOverride) =>
                Logger[F].warn(s"Overriding epoch length to $lengthOverride slots") >>
                protocol
                  .copy(epochLengthOverride = appConfig.bifrost.protocols(0).epochLengthOverride)
                  .pure[F]
              )
          )
          .flatTap(p => p.validation.leftMap(new IllegalArgumentException(_)).pure[F].rethrow)
          .toResource
      _ <- Logger[F]
        .info(
          s"Protocol" +
          s" epochLength=${bigBangProtocol.epochLength}" +
          s" operationalPeriodLength=${bigBangProtocol.operationalPeriodLength}" +
          s" chainSelectionSWindow=${bigBangProtocol.chainSelectionSWindow}" +
          s" settings=$bigBangProtocol"
        )
        .toResource
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
      _          <- Logger[F].info(show"globalSlot=$globalSlot").toResource
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

      epochBoundariesStateLocal <- EpochBoundariesEventSourcedState
        .make[F](
          clock,
          currentEventIdGetterSetters.epochBoundariesLocal.get(),
          blockIdTree,
          currentEventIdGetterSetters.epochBoundariesLocal.set,
          dataStores.epochBoundariesLocal.pure[F],
          dataStores.slotData.getOrRaise
        )
        .toResource
      consensusDataStateLocal <-
        ConsensusDataEventSourcedState
          .make[F](
            currentEventIdGetterSetters.consensusDataLocal.get(),
            blockIdTree,
            currentEventIdGetterSetters.consensusDataLocal.set,
            ConsensusDataEventSourcedState
              .ConsensusData(dataStores.activeStakeLocal, dataStores.inactiveStakeLocal, dataStores.registrationsLocal)
              .pure[F],
            dataStores.bodies.getOrRaise,
            dataStores.transactions.getOrRaise
          )
          .toResource
      consensusValidationStateLocal <- ConsensusValidationState
        .make[F](bigBangBlockId, epochBoundariesStateLocal, consensusDataStateLocal, clock)
        .toResource

      epochBoundariesStateP2P <- EpochBoundariesEventSourcedState
        .make[F](
          clock,
          currentEventIdGetterSetters.epochBoundariesP2P.get(),
          blockIdTree,
          currentEventIdGetterSetters.epochBoundariesP2P.set,
          dataStores.epochBoundariesP2P.pure[F],
          dataStores.slotData.getOrRaise
        )
        .toResource
      consensusDataStateP2P <-
        ConsensusDataEventSourcedState
          .make[F](
            currentEventIdGetterSetters.consensusDataP2P.get(),
            blockIdTree,
            currentEventIdGetterSetters.consensusDataP2P.set,
            ConsensusDataEventSourcedState
              .ConsensusData(dataStores.activeStakeP2P, dataStores.inactiveStakeP2P, dataStores.registrationsP2P)
              .pure[F],
            dataStores.bodies.getOrRaise,
            dataStores.transactions.getOrRaise
          )
          .toResource
      consensusValidationStateP2P <- ConsensusValidationState
        .make[F](bigBangBlockId, epochBoundariesStateP2P, consensusDataStateP2P, clock)
        .toResource

      chainSelectionAlgebra = ChainSelection
        .make[F](
          dataStores.slotData.getOrRaise,
          cryptoResources.blake2b512,
          bigBangProtocol.chainSelectionKLookback,
          bigBangProtocol.chainSelectionSWindow
        )
      localChain <-
        LocalChain.make(
          bigBangSlotData,
          canonicalHeadSlotData,
          chainSelectionAlgebra,
          currentEventIdGetterSetters.canonicalHead.set
        )
      staking =
        OptionT
          .liftF(StakingInit.stakingIsInitialized[F](stakingDir).toResource)
          .filter(identity)
          .flatTapNone(Logger[F].warn("Staking directory is empty.  Continuing in relay-only mode.").toResource)
          .semiflatMap { _ =>
            // Construct a separate threshold calcualtor instance with a separate cache to avoid
            // polluting the staker's cache with remote block eligibilities
            makeLeaderElectionThreshold(cryptoResources.blake2b512, vrfConfig).toResource
              .flatMap(leaderElectionThreshold =>
                StakingInit
                  .makeStakingFromDisk(
                    stakingDir,
                    appConfig.bifrost.staking.rewardAddress,
                    appConfig.bifrost.staking.stakingAddress,
                    clock,
                    etaCalculation,
                    consensusValidationStateLocal,
                    leaderElectionThreshold,
                    cryptoResources,
                    bigBangProtocol,
                    vrfConfig,
                    bigBangBlock.header.version,
                    metadata,
                    localChain,
                    dataStores.headers.getOrRaise,
                    dataStores.bodies.getOrRaise,
                    dataStores.transactions.getOrRaise
                  )
              )
          }
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
      (boxStateLocal, boxStateStateLocal) <- BoxState
        .make(
          currentEventIdGetterSetters.boxStateLocal.get(),
          dataStores.bodies.getOrRaise,
          dataStores.transactions.getOrRaise,
          blockIdTree,
          currentEventIdGetterSetters.boxStateLocal.set,
          dataStores.spendableBoxIdsLocal.pure[F]
        )
        .toResource
      (boxStateP2P, boxStateStateP2P) <- BoxState
        .make(
          currentEventIdGetterSetters.boxStateP2P.get(),
          dataStores.bodies.getOrRaise,
          dataStores.transactions.getOrRaise,
          blockIdTree,
          currentEventIdGetterSetters.boxStateP2P.set,
          dataStores.spendableBoxIdsP2P.pure[F]
        )
        .toResource
      (registrationAccumulatorLocal, registrationAccumulatorStateLocal) <- RegistrationAccumulator
        .make[F](
          currentEventIdGetterSetters.registrationAccumulatorLocal.get(),
          dataStores.bodies.getOrRaise,
          dataStores.transactions.getOrRaise,
          blockIdTree,
          currentEventIdGetterSetters.registrationAccumulatorLocal.set,
          dataStores.registrationAccumulatorLocal.pure[F]
        )
      (registrationAccumulatorP2P, registrationAccumulatorStateP2P) <- RegistrationAccumulator
        .make[F](
          currentEventIdGetterSetters.registrationAccumulatorP2P.get(),
          dataStores.bodies.getOrRaise,
          dataStores.transactions.getOrRaise,
          blockIdTree,
          currentEventIdGetterSetters.registrationAccumulatorP2P.set,
          dataStores.registrationAccumulatorP2P.pure[F]
        )
      validatorsLocal <- Validators.make[F](
        cryptoResources,
        dataStores,
        bigBangBlockId,
        eligibilityCache,
        etaCalculation,
        consensusValidationStateLocal,
        leaderElectionThreshold,
        clock,
        boxStateLocal,
        registrationAccumulatorLocal
      )
      validatorsP2P <- Validators.make[F](
        cryptoResources,
        dataStores,
        bigBangBlockId,
        eligibilityCache,
        etaCalculation,
        consensusValidationStateP2P,
        leaderElectionThreshold,
        clock,
        boxStateP2P,
        registrationAccumulatorP2P
      )
      genusOpt <- OptionT
        .whenF(appConfig.genus.enable)(
          Genus
            .make[F](
              appConfig.bifrost.rpc.bindHost,
              appConfig.bifrost.rpc.bindPort,
              nodeRpcTls = false,
              Some(appConfig.genus.orientDbDirectory)
                .filterNot(_.isEmpty)
                .getOrElse(dataStores.baseDirectory./("orient-db").toString),
              appConfig.genus.orientDbPassword
            )
        )
        .value
      genusServices <- genusOpt.toList.flatTraverse(genus =>
        GenusGrpc.Server.services(
          genus.blockFetcher,
          genus.transactionFetcher,
          genus.vertexFetcher,
          genus.valueFetcher
        )
      )
      healthCheck    <- HealthCheck.make[F]()
      healthServices <- HealthCheckGrpc.Server.services(healthCheck.healthChecker)

      protocolConfig <- ProtocolConfiguration.make[F](List(bigBangProtocol.nodeConfig(0)))

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
        epochBoundariesStateLocal,
        consensusDataStateLocal
      )

      epochData <- EpochDataInterpreter
        .make[F](Sync[F].defer(localChain.head).map(_.slotId.blockId), epochDataEventSourcedState)

      softwareVersion <- OptionT
        .whenF(appConfig.bifrost.versionInfo.enable)(
          VersionReplicator.make[F](metadata, appConfig.bifrost.versionInfo.uri)
        )
        .value

      costCalculator = TransactionCostCalculatorInterpreter.make[F](TransactionCostConfig())
      (mempool, mempoolState) <- Mempool.make[F](
        currentEventIdGetterSetters.mempool.get(),
        dataStores.bodies.getOrRaise,
        dataStores.transactions.getOrRaise,
        blockIdTree,
        currentEventIdGetterSetters.mempool.set,
        clock,
        id => Logger[F].info(show"Expiring transaction id=$id"),
        appConfig.bifrost.mempool.defaultExpirationSlots,
        transactionRewardCalculator,
        costCalculator
      )

      protectedMempool <- MempoolProtected.make(
        mempool,
        validatorsP2P.transactionSemantics,
        validatorsP2P.transactionAuthorization,
        currentEventIdGetterSetters.canonicalHead.get().flatMap(dataStores.headers.getOrRaise),
        dataStores.transactions.getOrRaise,
        transactionRewardCalculator,
        costCalculator,
        appConfig.bifrost.mempool.protection
      )

      eventSourcedStates = EventSourcedStates[F](
        epochDataEventSourcedState,
        blockHeightTreeLocal,
        blockHeightTreeP2P,
        consensusDataStateLocal,
        consensusDataStateP2P,
        epochBoundariesStateLocal,
        epochBoundariesStateP2P,
        boxStateStateLocal,
        boxStateStateP2P,
        mempoolState,
        registrationAccumulatorStateLocal,
        registrationAccumulatorStateP2P,
        txIdToBlockIdTree
      )

      _ <- Logger[F].info(show"Updating EventSourcedStates to id=$canonicalHeadId").toResource
      _ <- eventSourcedStates
        .updateAllStatesTo(canonicalHeadId)
        .logDuration("EventSourcedStates Update")
        .warnIfSlow("EventSourcedStates Update", 5.seconds)
        .toResource

      p2pConfig = appConfig.bifrost.p2p
      // Finally, run the program
      _ <- Blockchain
        .make[F](
          clock,
          staking,
          dataStores,
          localChain,
          chainSelectionAlgebra,
          blockIdTree,
          eventSourcedStates,
          validatorsLocal,
          validatorsP2P,
          protectedMempool,
          cryptoResources,
          localPeer,
          p2pConfig.knownPeers,
          appConfig.bifrost.rpc.bindHost,
          appConfig.bifrost.rpc.bindPort,
          protocolConfig,
          genusServices ::: healthServices,
          epochData,
          (p2pConfig.publicHost, p2pConfig.publicPort).mapN(KnownPeer),
          p2pConfig.networkProperties,
          costCalculator
        )
        .parProduct(genusOpt.traverse(Replicator.background[F]).void)
        .parProduct(
          softwareVersion.traverse(VersionReplicator.background[F](_, appConfig.bifrost.versionInfo.period)).void
        )
    } yield ()
  // scalastyle:on method.length

  private def makeLeaderElectionThreshold(blake2b512Resource: Resource[F, Blake2b512], vrfConfig: VrfConfig) =
    for {
      exp   <- ExpInterpreter.make[F](10000, 38)
      log1p <- Log1pInterpreter.make[F](10000, 8).flatMap(Log1pInterpreter.makeCached[F])
      base = LeaderElectionValidation.make[F](vrfConfig, blake2b512Resource, exp, log1p)
      leaderElectionThresholdCached <- LeaderElectionValidation.makeCached(base)
      leaderElectionThreshold = LeaderElectionValidation.makeWithCappedSlotDiff(
        leaderElectionThresholdCached,
        vrfConfig.lddCutoff
      )
    } yield leaderElectionThreshold

}

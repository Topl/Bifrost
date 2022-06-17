package co.topl.demo

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.stream.scaladsl.Source
import akka.util.Timeout
import cats.data.OptionT
import cats.effect.implicits._
import cats.effect.kernel.Sync
import cats.effect.unsafe.{IORuntime, IORuntimeConfig}
import cats.effect.{Async, ExitCode, IO, IOApp}
import cats.implicits._
import co.topl.algebras.ClockAlgebra.implicits.ClockOps
import co.topl.algebras._
import co.topl.catsakka._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.consensus.LeaderElectionValidation.VrfConfig
import co.topl.consensus._
import co.topl.consensus.algebras.{EtaCalculationAlgebra, LeaderElectionValidationAlgebra, LocalChainAlgebra}
import co.topl.crypto.hash.{Blake2b256, Blake2b512}
import co.topl.crypto.signing.{Ed25519, Ed25519VRF, KesProduct}
import co.topl.interpreters._
import co.topl.ledger.algebras.MempoolAlgebra
import co.topl.ledger.interpreters.{Mempool, TransactionSyntaxValidation}
import co.topl.minting._
import co.topl.minting.algebras.PerpetualBlockMintAlgebra
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility._
import co.topl.networking.p2p.{DisconnectedPeer, LocalPeer}
import co.topl.numerics.{ExpInterpreter, Log1pInterpreter}
import co.topl.typeclasses._
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.net.InetSocketAddress
import java.nio.file.{Files, Paths}
import java.security.SecureRandom
import java.time.Instant
import java.util.UUID
import scala.concurrent.duration._
import scala.util.Random

/**
 * Command-line args:
 *
 * port rpcPort [remotes] seed stakerCount stakerIndex ?genesisTimestampMs
 * i.e.: 9094 8094 [localhost:9095] 2348921 10 3 1648049271191
 */
object TetraDemo extends IOApp {

  // Configuration Data
  private val vrfConfig =
    VrfConfig(lddCutoff = 40, precision = 40, baselineDifficulty = Ratio(1, 20), amplitude = Ratio(2, 5))

  private val OperationalPeriodLength = 180L
  private val OperationalPeriodsPerEpoch = 4L
  private val EpochLength = OperationalPeriodLength * OperationalPeriodsPerEpoch
  private val SlotDuration = 100.milli

  require(
    EpochLength % OperationalPeriodLength === 0L,
    "EpochLength must be evenly divisible by OperationalPeriodLength"
  )

  private val ChainSelectionKLookback = 5_000L
  private val ChainSelectionSWindow = 200_000L

  private val KesKeyHeight = (9, 9)

  private def computeStakers(count: Int, random: Random) = List.tabulate(count) { idx =>
    implicit val ed25519Vrf: Ed25519VRF = Ed25519VRF.precomputed()
    implicit val ed25519: Ed25519 = new Ed25519
    implicit val kesProduct: KesProduct = new KesProduct
    val seed = Sized.strictUnsafe[Bytes, Lengths.`32`.type](Bytes(random.nextBytes(32)))

    val (_, poolVK) = new Ed25519().createKeyPair(seed)

    val (stakerVrfKey, _) =
      ed25519Vrf.createKeyPair(seed)

    val (kesKey, _) =
      kesProduct.createKeyPair(seed = seed.data, height = KesKeyHeight, 0)

    val stakerRegistration: Box.Values.Registrations.Operator =
      Box.Values.Registrations.Operator(
        vrfCommitment = kesProduct.sign(
          kesKey,
          new Blake2b256().hash(ed25519Vrf.getVerificationKey(stakerVrfKey).immutableBytes, poolVK.bytes.data).data
        )
      )

    Staker(Ratio(1, count), stakerVrfKey, kesKey, stakerRegistration, StakingAddresses.Operator(poolVK))
  }

  private val genesis =
    BlockGenesis(Nil).value

  // Actor system initialization

  implicit private val system: ActorSystem[_] =
    ActorSystem(Behaviors.empty, "TetraDemo")

  override val runtime: IORuntime = AkkaCatsRuntime(system).runtime
  override val runtimeConfig: IORuntimeConfig = AkkaCatsRuntime(system).ioRuntimeConfig

  // Interpreter initialization

  type F[A] = IO[A]

  implicit private val logger: Logger[F] = Slf4jLogger.getLogger[F]

  private def makeClock(args: DemoArgs): ClockAlgebra[F] =
    SchedulerClock.Eval.make(SlotDuration, EpochLength, args.genesisTimestamp)

  implicit private val timeout: Timeout = Timeout(20.seconds)

  private def state(stakers: List[Staker]): F[ConsensusStateReader[F]] =
    NodeViewHolder.StaticData.make[F](
      stakers.map(staker => staker.address -> staker.relativeStake).toMap,
      stakers.map(staker => staker.address -> staker.registration).toMap
    )

  private val statsDir = Paths.get(".bifrost", "stats")
  Files.createDirectories(statsDir)

  private val statsInterpreter =
    StatsInterpreter.Eval.make[F](statsDir)

  private def createMint(
    staker:                  Staker,
    clock:                   ClockAlgebra[F],
    etaCalculation:          EtaCalculationAlgebra[F],
    leaderElectionThreshold: LeaderElectionValidationAlgebra[F],
    localChain:              LocalChainAlgebra[F],
    mempool:                 MempoolAlgebra[F],
    headerStore:             Store[F, TypedIdentifier, BlockHeaderV2],
    fetchTransaction:        TypedIdentifier => F[Transaction],
    state:                   ConsensusStateReader[F],
    ed25519VRFResource:      UnsafeResource[F, Ed25519VRF],
    kesProductResource:      UnsafeResource[F, KesProduct],
    ed25519Resource:         UnsafeResource[F, Ed25519]
  ): F[PerpetualBlockMintAlgebra[F]] =
    for {
      _            <- Logger[F].info(show"Initializing staker key idx=0 address=${staker.address}")
      stakerKeyDir <- IO.blocking(Files.createTempDirectory(show"TetraDemoStaker${staker.address}"))
      secureStore  <- AkkaSecureStore.Eval.make[F](stakerKeyDir)
      _            <- secureStore.write(UUID.randomUUID().toString, staker.kesKey)
      vrfProofConstruction <- VrfProof.Eval.make[F](
        staker.vrfKey,
        clock,
        leaderElectionThreshold,
        ed25519VRFResource,
        vrfConfig
      )
      initialSlot  <- clock.globalSlot.map(_.max(0L))
      initialEpoch <- clock.epochOf(initialSlot)
      _            <- vrfProofConstruction.precomputeForEpoch(initialEpoch, genesis.headerV2.eligibilityCertificate.eta)
      operationalKeys <- OperationalKeys.FromSecureStore.make[F](
        secureStore = secureStore,
        clock = clock,
        vrfProof = vrfProofConstruction,
        etaCalculation,
        state,
        kesProductResource,
        ed25519Resource,
        genesis.headerV2.slotId,
        operationalPeriodLength = OperationalPeriodLength,
        activationOperationalPeriod = 0L,
        staker.address,
        initialSlot = initialSlot
      )
      stakerVRFVK <- ed25519VRFResource.use(_.getVerificationKey(staker.vrfKey).pure[F])
      mint =
        BlockMint.Eval.make(
          Staking.Eval.make(
            staker.address,
            LeaderElectionMinting.Eval.make(
              stakerVRFVK,
              leaderElectionThreshold,
              vrfProofConstruction,
              statsInterpreter = StatsInterpreter.Noop.make[F],
              statsName = ""
            ),
            operationalKeys,
            VrfRelativeStakeMintingLookup.Eval.make(state, clock),
            etaCalculation,
            ed25519Resource,
            vrfProofConstruction,
            clock
          ),
          clock,
          statsInterpreter
        )
      perpetual <- PerpetualBlockMint.InAkkaStream
        .make(clock, mint, localChain, mempool, headerStore, fetchTransaction)
    } yield perpetual

  // Program definition

  def run(args: List[String]): IO[ExitCode] = {
    for {
      demoArgs <- DemoArgs.parse(args).pure[F]
      random = new Random(demoArgs.seed)
      stakers = computeStakers(demoArgs.stakerCount, random)
      blake2b256Resource <- ActorPoolUnsafeResource.Eval.make[F, Blake2b256](new Blake2b256, _ => ())
      blake2b512Resource <- ActorPoolUnsafeResource.Eval.make[F, Blake2b512](new Blake2b512, _ => ())
      ed25519VRFResource <- ActorPoolUnsafeResource.Eval.make[F, Ed25519VRF](Ed25519VRF.precomputed(), _ => ())
      kesProductResource <- ActorPoolUnsafeResource.Eval.make[F, KesProduct](new KesProduct, _ => ())
      ed25519Resource    <- ActorPoolUnsafeResource.Eval.make[F, Ed25519](new Ed25519, _ => ())
      blockHeaderStore   <- RefStore.Eval.make[F, TypedIdentifier, BlockHeaderV2]()
      blockBodyStore     <- RefStore.Eval.make[F, TypedIdentifier, BlockBodyV2]()
      transactionStore   <- RefStore.Eval.make[F, TypedIdentifier, Transaction]()
      _                  <- blockHeaderStore.put(genesis.headerV2.id, genesis.headerV2)
      _                  <- blockBodyStore.put(genesis.headerV2.id, genesis.blockBodyV2)
      slotDataCache      <- SlotDataCache.Eval.make(blockHeaderStore, ed25519VRFResource)
      slotDataStore = blockHeaderStore
        .mapRead[TypedIdentifier, SlotData](identity, _.slotData(Ed25519VRF.precomputed()))
      blockIdTree                 <- BlockIdTree.make[F]
      _                           <- blockIdTree.associate(genesis.headerV2.id, genesis.headerV2.parentHeaderId)
      blockHeightTreeStore        <- RefStore.Eval.make[F, Long, TypedIdentifier]()
      blockHeightTreeUnapplyStore <- RefStore.Eval.make[F, TypedIdentifier, Long]()
      blockHeightTree <- BlockHeightTree
        .make[F](
          blockHeightTreeStore,
          genesis.headerV2.parentHeaderId,
          slotDataStore,
          blockHeightTreeUnapplyStore,
          blockIdTree
        )
      clock = makeClock(demoArgs)
      etaCalculation <- EtaCalculation.Eval.make(
        slotDataCache,
        clock,
        genesis.headerV2.eligibilityCertificate.eta,
        blake2b256Resource,
        blake2b512Resource
      )
      consensusState <- state(stakers)
      exp            <- ExpInterpreter.make[F](10000, 38)
      log1p          <- Log1pInterpreter.make[F](10000, 8)
      log1pCached    <- Log1pInterpreter.makeCached[F](log1p)
      leaderElectionThreshold = LeaderElectionValidation.Eval.make[F](vrfConfig, blake2b512Resource, exp, log1pCached)
      underlyingHeaderValidation <- BlockHeaderValidation.Eval.make[F](
        etaCalculation,
        VrfRelativeStakeValidationLookup.Eval.make(consensusState, clock),
        leaderElectionThreshold,
        RegistrationLookup.Eval.make(consensusState, clock),
        ed25519VRFResource,
        kesProductResource,
        ed25519Resource,
        blake2b256Resource
      )
      cachedHeaderValidation <- BlockHeaderValidation.WithCache.make[F](underlyingHeaderValidation, blockHeaderStore)
      localChain <- LocalChain.Eval.make(
        genesis.headerV2.slotData(Ed25519VRF.precomputed()),
        ChainSelection.orderT[F](slotDataCache, blake2b512Resource, ChainSelectionKLookback, ChainSelectionSWindow)
      )
      syntacticValidation <- TransactionSyntaxValidation.make[F]
      mempool <- Mempool.make[F](
        genesis.headerV2.id.asTypedBytes.pure[F],
        blockBodyStore.getOrRaise,
        transactionStore.getOrRaise,
        blockIdTree,
        clock,
        id => Logger[F].info(show"Expiring transaction id=$id"),
        Long.MaxValue,
        1000L
      )
      mintOpt <- OptionT
        .fromOption[F](demoArgs.stakerIndex)
        .semiflatMap(idx =>
          createMint(
            stakers(idx),
            clock,
            etaCalculation,
            leaderElectionThreshold,
            localChain,
            mempool,
            blockHeaderStore,
            transactionStore.getOrRaise,
            consensusState,
            ed25519VRFResource,
            kesProductResource,
            ed25519Resource
          )
        )
        .value
      implicit0(networkRandom: Random) = new Random(new SecureRandom())
      _ <- DemoProgram
        .run[F](
          mintOpt,
          cachedHeaderValidation,
          blockHeaderStore,
          blockBodyStore,
          transactionStore,
          slotDataStore,
          localChain,
          blockIdTree,
          blockHeightTree,
          ed25519VRFResource,
          "localhost",
          demoArgs.port,
          LocalPeer(InetSocketAddress.createUnresolved("localhost", demoArgs.port), (0, 0)),
          Source(demoArgs.remotes).delay(2.seconds).concat(Source.never),
          (_, flow) => flow,
          syntacticValidation,
          mempool,
          "localhost",
          demoArgs.rpcPort
        )
    } yield ()
  }
    .guarantee(
      Sync[F].delay(system.terminate()).flatMap(_ => Async[F].fromFuture(system.whenTerminated.pure[F])).void
    )
    .as(ExitCode.Success)
}

private case class Staker(
  relativeStake: Ratio,
  vrfKey:        SecretKeys.VrfEd25519,
  kesKey:        SecretKeys.KesProduct,
  registration:  Box.Values.Registrations.Operator,
  address:       StakingAddresses.Operator
)

case class DemoArgs(
  port:             Int,
  rpcPort:          Int,
  remotes:          List[DisconnectedPeer],
  seed:             Long,
  stakerCount:      Int,
  stakerIndex:      Option[Int],
  genesisTimestamp: Instant
)

object DemoArgs {

  def parse(args: List[String]): DemoArgs =
    DemoArgs(
      args(0).toInt,
      args(1).toInt,
      args(1)
        .drop(1)
        .dropRight(1)
        .split(',')
        .toList
        .filter(_.nonEmpty)
        .map(_.split(':'))
        .map(arr => DisconnectedPeer(InetSocketAddress.createUnresolved(arr(0), arr(1).toInt), (0, 0))),
      args(2).toLong,
      args(3).toInt,
      Option(args(4).toInt).filterNot(_ < 0),
      Instant.ofEpochMilli(args.lift(5).fold(defaultGenesisTimestamp())(a => a.toLong))
    )

  private def defaultGenesisTimestamp() = {
    val i = Instant.now()
    val t = i.toEpochMilli
    val result = t - (t % 10_000L) + 20_000L
    println(s"Initializing default genesisTimestamp=$result.  current=$t")
    result
  }
}

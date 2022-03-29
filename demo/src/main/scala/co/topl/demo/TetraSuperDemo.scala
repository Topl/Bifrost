package co.topl.demo

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.stream.scaladsl.Source
import akka.util.Timeout
import cats.arrow.FunctionK
import cats.data.OptionT
import cats.effect.implicits._
import cats.effect.kernel.Sync
import cats.effect.unsafe.{IORuntime, IORuntimeConfig}
import cats.effect.{Async, ExitCode, IO, IOApp}
import cats.implicits._
import cats.~>
import co.topl.algebras.ClockAlgebra.implicits.ClockOps
import co.topl.algebras._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.consensus.LeaderElectionValidation.VrfConfig
import co.topl.consensus._
import co.topl.consensus.algebras.{EtaCalculationAlgebra, LeaderElectionValidationAlgebra, LocalChainAlgebra}
import co.topl.crypto.hash.{blake2b256, Blake2b256, Blake2b512}
import co.topl.crypto.signing.{Ed25519, Ed25519VRF, KesProduct}
import co.topl.interpreters._
import co.topl.minting._
import co.topl.minting.algebras.PerpetualBlockMintAlgebra
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility._
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
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Random

/**
 * Runs multiple independent DemoProgram instances, each with their own storage
 * and networking.  The instances share the same JVM, but they still communicate
 * over TCP and they each bind to an independent port on localhost.
 */
object TetraSuperDemo extends IOApp {

  // Configuration Data
  private val vrfConfig =
    VrfConfig(lddCutoff = 40, precision = 40, baselineDifficulty = Ratio(1, 20), amplitude = Ratio(2, 5))

  private val OperationalPeriodLength = 180L
  private val OperationalPeriodsPerEpoch = 4L
  private val EpochLength = OperationalPeriodLength * OperationalPeriodsPerEpoch
  private val SlotDuration = 200.milli

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

    val stakerRegistration: Box.Values.TaktikosRegistration =
      Box.Values.TaktikosRegistration(
        commitment = kesProduct.sign(
          kesKey,
          new Blake2b256().hash(ed25519Vrf.getVerificationKey(stakerVrfKey).immutableBytes, poolVK.bytes.data).data
        )
      )

    val stakerAddress: TaktikosAddress = {
      val (paymentKey, paymentVerificationKey) = ed25519.createKeyPair(seed)
      TaktikosAddress(
        Sized.strictUnsafe(
          Bytes(blake2b256.hash(paymentVerificationKey.bytes.data.toArray).value)
        ),
        poolVK,
        ed25519.sign(paymentKey, poolVK.bytes.data)
      )
    }
    Staker(Ratio(1, count), stakerVrfKey, kesKey, stakerRegistration, stakerAddress)
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

  private def makeClock(genesisTimestamp: Instant): ClockAlgebra[F] =
    AkkaSchedulerClock.Eval.make(SlotDuration, EpochLength, genesisTimestamp)

  implicit private val timeout: Timeout = Timeout(20.seconds)

  private def state(stakers: List[Staker]): F[ConsensusStateReader[F]] =
    NodeViewHolder.StaticData.make[F](
      stakers.map(staker => staker.address -> staker.relativeStake).toMap,
      stakers.map(staker => staker.address -> staker.registration).toMap
    )

  private val statsDir = Paths.get(".bifrost", "stats")
  Files.createDirectories(statsDir)

  private def statsInterpreter =
    StatsInterpreter.Eval.make[F](statsDir)

  private def createMint(
    staker:                  Staker,
    clock:                   ClockAlgebra[F],
    etaCalculation:          EtaCalculationAlgebra[F],
    leaderElectionThreshold: LeaderElectionValidationAlgebra[F],
    localChain:              LocalChainAlgebra[F],
    mempool:                 MemPoolAlgebra[F],
    headerStore:             Store[F, BlockHeaderV2],
    state:                   ConsensusStateReader[F],
    ed25519VRFResource:      UnsafeResource[F, Ed25519VRF],
    kesProductResource:      UnsafeResource[F, KesProduct],
    ed25519Resource:         UnsafeResource[F, Ed25519]
  )(implicit logger:         Logger[F]): F[PerpetualBlockMintAlgebra[F]] =
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
        .make(clock, mint, localChain, mempool, headerStore)
    } yield perpetual

  // Program definition

  implicit val fToIo: F ~> IO = FunctionK.id

  implicit val fToFuture: F ~> Future = FunctionK.liftFunction(_.unsafeToFuture()(runtime))

  private val loggerColors = List(
    Console.MAGENTA,
    Console.BLUE,
    Console.YELLOW,
    Console.GREEN,
    Console.CYAN,
    Console.RED,
    Console.MAGENTA_B,
    Console.BLUE_B,
    Console.YELLOW_B,
    Console.GREEN_B,
    Console.CYAN_B,
    Console.RED_B
  )

  def run(args: List[String]): IO[ExitCode] = {
    for {
      random <- new Random(0L).pure[F]
      genesisTimestamp = Instant.now().plusSeconds(10)
      stakerCount = 3
      stakers = computeStakers(stakerCount, random)
      configurations = List.tabulate(stakerCount)(idx =>
        (
          idx + 9090,
          if (idx > 0) List(InetSocketAddress.createUnresolved("localhost", 9090 + idx - 1))
          else List(InetSocketAddress.createUnresolved("localhost", 9090 + idx + stakerCount - 1)),
          idx,
          true
        )
      )
      _ <- configurations.parTraverse { case (port, remotes, stakerIndex, stakingEnabled) =>
        runInstance(port, remotes, stakers, stakerIndex, stakingEnabled, genesisTimestamp)
      }
    } yield ()
  }
    .guarantee(
      Sync[F].delay(system.terminate()).flatMap(_ => Async[F].fromFuture(system.whenTerminated.pure[F])).void
    )
    .as(ExitCode.Success)

  private def runInstance(
    port:             Int,
    remotes:          List[InetSocketAddress],
    stakers:          List[Staker],
    stakerIndex:      Int,
    stakingEnabled:   Boolean,
    genesisTimestamp: Instant
  ) =
    Sync[F].defer(
      for {
        blake2b256Resource <- ActorPoolUnsafeResource.Eval.make[F, Blake2b256](new Blake2b256, _ => ())
        blake2b512Resource <- ActorPoolUnsafeResource.Eval.make[F, Blake2b512](new Blake2b512, _ => ())
        ed25519VRFResource <- ActorPoolUnsafeResource.Eval.make[F, Ed25519VRF](Ed25519VRF.precomputed(), _ => ())
        kesProductResource <- ActorPoolUnsafeResource.Eval.make[F, KesProduct](new KesProduct, _ => ())
        ed25519Resource    <- ActorPoolUnsafeResource.Eval.make[F, Ed25519](new Ed25519, _ => ())
        loggerColor = loggerColors(stakerIndex).toString
        implicit0(logger: Logger[F]) = Slf4jLogger
          .getLoggerFromName[F](s"${loggerColor}TetraSuperDemo$stakerIndex${Console.RESET}")
          .withModifiedString(str => s"$loggerColor$str${Console.RESET}")
        blockHeaderStore <- RefStore.Eval.make[F, BlockHeaderV2]()
        blockBodyStore   <- RefStore.Eval.make[F, BlockBodyV2]()
        transactionStore <- RefStore.Eval.make[F, Transaction]()
        _                <- blockHeaderStore.put(genesis.headerV2.id, genesis.headerV2)
        _                <- blockBodyStore.put(genesis.headerV2.id, genesis.blockBodyV2)
        slotDataCache    <- SlotDataCache.Eval.make(blockHeaderStore, ed25519VRFResource)
        clock = makeClock(genesisTimestamp)
        etaCalculation <- EtaCalculation.Eval.make(
          slotDataCache,
          clock,
          genesis.headerV2.eligibilityCertificate.eta,
          blake2b256Resource,
          blake2b512Resource
        )
        exp         <- ExpInterpreter.make[F](10000, 38)
        log1p       <- Log1pInterpreter.make[F](10000, 8)
        log1pCached <- Log1pInterpreter.makeCached[F](log1p)
        leaderElectionThreshold = LeaderElectionValidation.Eval.make[F](vrfConfig, blake2b512Resource, exp, log1pCached)
        consensusState <- state(stakers)
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
        mempool <- EmptyMemPool.make[F]
        mintOpt <- OptionT
          .whenF(stakingEnabled)(
            createMint(
              stakers(stakerIndex),
              clock,
              etaCalculation,
              leaderElectionThreshold,
              localChain,
              mempool,
              blockHeaderStore,
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
            localChain,
            ed25519VRFResource,
            port,
            Source.single(remotes).delay(2.seconds).mapConcat(identity).concat(Source.never)
          )
      } yield ()
    )
}

package co.topl.simulator.eligibility

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.util.Timeout
import cats.arrow.FunctionK
import cats.data.{Chain, OptionT}
import cats.effect.implicits._
import cats.effect.kernel.Sync
import cats.effect.unsafe.{IORuntime, IORuntimeConfig}
import cats.effect.{Async, IO, IOApp}
import cats.implicits._
import cats.~>
import co.topl.algebras._
import co.topl.blockchain.{BigBang, StakerInitializers}
import co.topl.catsakka._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.consensus.LeaderElectionValidation.VrfConfig
import co.topl.consensus._
import co.topl.consensus.algebras.{
  ConsensusValidationStateAlgebra,
  EtaCalculationAlgebra,
  LeaderElectionValidationAlgebra
}
import co.topl.crypto.generation.mnemonic.Entropy
import co.topl.crypto.hash.{Blake2b256, Blake2b512}
import co.topl.crypto.signing.{Ed25519, Ed25519VRF, KesProduct}
import co.topl.interpreters._
import co.topl.minting._
import co.topl.minting.algebras.BlockMintAlgebra
import co.topl.models.Box.Values.Registrations
import co.topl.models._
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility._
import co.topl.numerics.{ExpInterpreter, Log1pInterpreter}
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.nio.file.{Files, Paths}
import java.time.Instant
import java.util.UUID
import scala.concurrent.duration._
import scala.util.Random

object EligibilitySimulator extends IOApp.Simple {

  // Configuration Data
  private val vrfConfig =
    VrfConfig(lddCutoff = 10, precision = 38, baselineDifficulty = Ratio(1, 20), amplitude = Ratio(1))

  private val OperationalPeriodLength = 180L
  private val OperationalPeriodsPerEpoch = 4L
  private val EpochLength = OperationalPeriodLength * OperationalPeriodsPerEpoch
  private val SlotDuration = 10.milli
  private val NumberOfStakers = 1
  private val TotalStake = NumberOfStakers
  private val TargetHeight = 10_000L
  private val TestName = "test_threshold"

  require(
    EpochLength % OperationalPeriodLength === 0L,
    "EpochLength must be evenly divisible by OperationalPeriodLength"
  )

  private val ChainSelectionKLookback = 5_000L
  private val ChainSelectionSWindow = 200_000L

  private val KesKeyHeight = (9, 9)

  // Create stubbed/sample/demo data

  private val (_, poolVK) =
    new Ed25519().deriveKeyPairFromEntropy(Entropy.fromUuid(UUID.randomUUID()), None)

  private val stakers = List.fill(NumberOfStakers) {
    StakerInitializers.Operator(
      Sized.strictUnsafe(Bytes(Random.nextBytes(32))),
      KesKeyHeight
    )
  }

  private val genesis =
    BigBang.block(BigBang.Config(0L, Chain.empty))

  // Actor system initialization

  implicit private val system: ActorSystem[_] = ActorSystem(Behaviors.empty, "EligibilitySimulator")

  override val runtime: IORuntime = AkkaCatsRuntime(system).runtime
  override val runtimeConfig: IORuntimeConfig = AkkaCatsRuntime(system).ioRuntimeConfig

  // Interpreter initialization

  type F[A] = IO[A]

  implicit private val logger: Logger[F] = Slf4jLogger.getLogger[F]

  implicit private val networkPrefix: NetworkPrefix = NetworkPrefix(1)

  private val clock: ClockAlgebra[F] =
    SchedulerClock.Eval.make(SlotDuration, EpochLength, Instant.now())

  implicit private val timeout: Timeout = Timeout(20.seconds)

  private val consensusValidationState: ConsensusValidationStateAlgebra[F] =
    new ConsensusValidationStateAlgebra[F] {
      private val registrations = stakers.map(staker => staker.stakingAddress -> staker.registration).toMap

      def operatorRelativeStake(currentBlockId: TypedIdentifier, slot: Slot)(
        address:                                StakingAddresses.Operator
      ): F[Option[Ratio]] = Ratio(1, NumberOfStakers).some.pure[F]

      def operatorRegistration(currentBlockId: TypedIdentifier, slot: Slot)(
        address:                               StakingAddresses.Operator
      ): F[Option[Registrations.Operator]] = registrations.get(address).pure[F]
    }

  private val statsDir = Paths.get(".bifrost", "stats")
  Files.createDirectories(statsDir)

  private val statsInterpreter =
    StatsInterpreter.Eval.make[F](statsDir)

  private def mints(
    etaCalculation:          EtaCalculationAlgebra[F],
    leaderElectionThreshold: LeaderElectionValidationAlgebra[F],
    state:                   ConsensusValidationStateAlgebra[F],
    ed25519VRFResource:      UnsafeResource[F, Ed25519VRF],
    kesProductResource:      UnsafeResource[F, KesProduct],
    ed25519Resource:         UnsafeResource[F, Ed25519]
  ): F[List[BlockMintAlgebra[F]]] =
    stakers
      .parTraverse(staker =>
        for {
          _           <- Logger[F].info(show"Initializing staker key idx=0 address=${staker.stakingAddress.show}")
          secureStore <- InMemorySecureStore.Eval.make[F]
          _           <- secureStore.write(UUID.randomUUID().toString, staker.kesSK)
          vrfProofConstruction <- VrfProof.Eval.make[F](
            staker.vrfSK,
            clock,
            leaderElectionThreshold,
            ed25519VRFResource,
            vrfConfig
          )
          _ <- vrfProofConstruction.precomputeForEpoch(0, genesis.headerV2.eligibilityCertificate.eta)
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
            staker.stakingAddress,
            initialSlot = 0L
          )
          mint =
            BlockMint.Eval.make(
              Staking.Eval.make(
                staker.stakingAddress,
                LeaderElectionMinting.Eval.make(
                  staker.vrfVK,
                  leaderElectionThreshold,
                  vrfProofConstruction,
                  statsInterpreter,
                  TestName + "Thresholds"
                ),
                operationalKeys,
                state,
                etaCalculation,
                ed25519Resource,
                vrfProofConstruction,
                clock
              ),
              clock,
              StatsInterpreter.Noop.make[F]
            )
        } yield mint
      )

  private def createBlockStore(
    headerStore: Store[F, TypedIdentifier, BlockHeaderV2],
    bodyStore:   Store[F, TypedIdentifier, BlockBodyV2]
  ): Store[F, TypedIdentifier, BlockV2] =
    new Store[F, TypedIdentifier, BlockV2] {

      def get(id: TypedIdentifier): F[Option[BlockV2]] =
        (OptionT(headerStore.get(id)), OptionT(bodyStore.get(id))).tupled.map((BlockV2.apply _).tupled).value

      def put(id: TypedIdentifier, t: BlockV2): F[Unit] =
        (headerStore.put(id, t.headerV2), bodyStore.put(id, t.blockBodyV2)).tupled.void

      def remove(id: TypedIdentifier): F[Unit] =
        (headerStore.remove(id), bodyStore.remove(id)).tupled.void

      def contains(id: TypedIdentifier): F[Boolean] = (headerStore.contains(id), bodyStore.contains(id)).mapN(_ && _)
    }

  // Program definition

  implicit val fToIo: ~>[F, IO] = FunctionK.id

  val run: IO[Unit] = {
    for {
      blake2b256Resource <- ActorPoolUnsafeResource.Eval.make[F, Blake2b256](new Blake2b256, _ => ())
      blake2b512Resource <- ActorPoolUnsafeResource.Eval.make[F, Blake2b512](new Blake2b512, _ => ())
      ed25519VRFResource <- ActorPoolUnsafeResource.Eval.make[F, Ed25519VRF](Ed25519VRF.precomputed(), _ => ())
      kesProductResource <- ActorPoolUnsafeResource.Eval.make[F, KesProduct](new KesProduct, _ => ())
      ed25519Resource    <- ActorPoolUnsafeResource.Eval.make[F, Ed25519](new Ed25519, _ => ())
      slotDataStore      <- RefStore.Eval.make[F, TypedIdentifier, SlotData]()
      blockHeaderStore   <- RefStore.Eval.make[F, TypedIdentifier, BlockHeaderV2]()
      blockBodyStore     <- RefStore.Eval.make[F, TypedIdentifier, BlockBodyV2]()
      _                  <- slotDataStore.put(genesis.headerV2.id, genesis.headerV2.slotData(Ed25519VRF.precomputed()))
      etaCalculation <- EtaCalculation.Eval.make(
        slotDataStore.getOrRaise,
        clock,
        genesis.headerV2.eligibilityCertificate.eta,
        blake2b256Resource,
        blake2b512Resource
      )
      exp         <- ExpInterpreter.make[F](10000, vrfConfig.precision)
      log1p       <- Log1pInterpreter.make[F](10000, 5)
      log1pCached <- Log1pInterpreter.makeCached[F](log1p)
      leaderElectionThreshold = LeaderElectionValidation.Eval.make[F](vrfConfig, blake2b512Resource, exp, log1pCached)
      leaderElectionThresholdCached <- LeaderElectionValidation.Eval.makeCached[F](leaderElectionThreshold)
      underlyingHeaderValidation <- BlockHeaderValidation.Eval.make[F](
        etaCalculation,
        consensusValidationState,
        leaderElectionThresholdCached,
        ed25519VRFResource,
        kesProductResource,
        ed25519Resource,
        blake2b256Resource
      )
      cachedHeaderValidation <- BlockHeaderValidation.WithCache.make[F](underlyingHeaderValidation, blockHeaderStore)
      localChain <- LocalChain.Eval.make(
        genesis.headerV2.slotData(Ed25519VRF.precomputed()),
        ChainSelection
          .orderT[F](slotDataStore.getOrRaise, blake2b512Resource, ChainSelectionKLookback, ChainSelectionSWindow)
      )
      m <- mints(
        etaCalculation,
        leaderElectionThresholdCached,
        consensusValidationState,
        ed25519VRFResource,
        kesProductResource,
        ed25519Resource
      )
      _ <- EligibilitySimulatorProgram
        .run[F](
          clock,
          m,
          cachedHeaderValidation,
          blockHeaderStore,
          etaCalculation,
          localChain,
          ed25519VRFResource,
          statsInterpreter,
          TestName,
          TargetHeight
        )
    } yield ()
  }
    .guarantee(
      Sync[F].delay(system.terminate()).flatMap(_ => Async[F].fromFuture(system.whenTerminated.pure[F])).void
    )
}

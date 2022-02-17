package co.topl.simulator.eligibility

import akka.actor.typed.ActorSystem
import akka.util.Timeout
import cats.arrow.FunctionK
import cats.data.OptionT
import cats.effect.implicits._
import cats.effect.kernel.Sync
import cats.effect.unsafe.{IORuntime, IORuntimeConfig}
import cats.effect.{Async, IO, IOApp}
import cats.implicits._
import cats.~>
import co.topl.algebras._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.consensus.LeaderElectionValidation.VrfConfig
import co.topl.consensus._
import co.topl.consensus.algebras.{EtaCalculationAlgebra, LeaderElectionValidationAlgebra}
import co.topl.crypto.hash.{Blake2b256, Blake2b512}
import co.topl.crypto.mnemonic.Entropy
import co.topl.crypto.signing.{Ed25519, Ed25519VRF, KesProduct}
import co.topl.interpreters._
import co.topl.minting._
import co.topl.minting.algebras.BlockMintAlgebra
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility._
import co.topl.typeclasses._
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger

import java.util.UUID
import scala.concurrent.duration._
import scala.util.Random

object EligibilitySimulator extends IOApp.Simple {

  // Configuration Data
  private val vrfConfig =
    VrfConfig(lddCutoff = 10, precision = 16, baselineDifficulty = Ratio(1, 20), amplitude = Ratio(1))
  //    VrfConfig(lddCutoff = 40, precision = 16, baselineDifficulty = Ratio(1, 20), amplitude = Ratio(2, 5))

  private val OperationalPeriodLength = 180L
  private val OperationalPeriodsPerEpoch = 4L
  private val EpochLength = OperationalPeriodLength * OperationalPeriodsPerEpoch
  private val SlotDuration = 10.milli

  require(
    EpochLength % OperationalPeriodLength === 0L,
    "EpochLength must be evenly divisible by OperationalPeriodLength"
  )

  private val ChainSelectionKLookback = 5_000L
  private val ChainSelectionSWindow = 200_000L

  private val KesKeyHeight = (9, 9)

  // Create stubbed/sample/demo data

  private val NumberOfStakers = 1
  private val RelativeStake = Ratio(1, NumberOfStakers)

  private val (_, poolVK) =
    new Ed25519().createKeyPair(Entropy.fromUuid(UUID.randomUUID()), None)

  private val stakers = List.fill(NumberOfStakers) {

    implicit val ed25519Vrf: Ed25519VRF = Ed25519VRF.precomputed()
    implicit val ed25519: Ed25519 = new Ed25519
    implicit val kesProduct: KesProduct = new KesProduct

    val (stakerVrfKey, _) =
      ed25519Vrf.createKeyPair(Entropy.fromUuid(UUID.randomUUID()), None)

    val (kesKey, _) =
      kesProduct.createKeyPair(seed = Bytes(Random.nextBytes(32)), height = KesKeyHeight, 0)

    val stakerRegistration: Box.Values.TaktikosRegistration =
      Box.Values.TaktikosRegistration(
        commitment = kesProduct.sign(
          kesKey,
          new Blake2b256().hash(ed25519Vrf.getVerificationKey(stakerVrfKey).immutableBytes, poolVK.bytes.data).data
        )
      )

    val stakerAddress: TaktikosAddress = {
      val (paymentKey, paymentVerificationKey) = ed25519.createKeyPair(Entropy.fromUuid(UUID.randomUUID()), None)
      TaktikosAddress(
        new Blake2b256().hash(paymentVerificationKey.bytes.data),
        poolVK,
        ed25519.sign(paymentKey, poolVK.bytes.data)
      )
    }
    Staker(RelativeStake, stakerVrfKey, kesKey, stakerRegistration, stakerAddress)
  }

  private val genesis =
    BlockGenesis(Nil).value

  private val initialNodeView =
    NodeView(
      Map(0L -> stakers.map(staker => staker.address -> staker.relativeStake).toMap),
      Map(0L -> stakers.map(staker => staker.address -> staker.registration).toMap)
    )

  // Actor system initialization

  implicit private val system: ActorSystem[NodeViewHolder.ReceivableMessage] =
    ActorSystem(
      NodeViewHolder(initialNodeView),
      "TetraDemo"
    )

  override val runtime: IORuntime = AkkaCatsRuntime(system).runtime
  override val runtimeConfig: IORuntimeConfig = AkkaCatsRuntime(system).ioRuntimeConfig

  // Interpreter initialization

  type F[A] = IO[A]

  implicit private val logger: Logger[F] =
    new Logger[F] {

      def error(t: Throwable)(message: => String): F[Unit] =
        Sync[F].delay(System.err.println(message + " " + t.toString))

      def warn(t: Throwable)(message: => String): F[Unit] =
        Sync[F].delay(System.out.println(message + " " + t.toString))

      def info(t: Throwable)(message: => String): F[Unit] =
        Sync[F].delay(System.out.println(message + " " + t.toString))

      def debug(t: Throwable)(message: => String): F[Unit] =
        Sync[F].delay(System.out.println(message + " " + t.toString))

      def trace(t: Throwable)(message: => String): F[Unit] =
        Sync[F].delay(System.out.println(message + " " + t.toString))

      def error(message: => String): F[Unit] =
        Sync[F].delay(System.err.println(message))

      def warn(message: => String): F[Unit] =
        Sync[F].delay(System.out.println(message))

      def info(message: => String): F[Unit] =
        Sync[F].delay(System.out.println(message))

      def debug(message: => String): F[Unit] =
        Sync[F].delay(System.out.println(message))

      def trace(message: => String): F[Unit] =
        Sync[F].delay(System.out.println(message))
    }

  private val clock: ClockAlgebra[F] =
    AkkaSchedulerClock.Eval.make(SlotDuration, EpochLength)

  implicit private val timeout: Timeout = Timeout(20.seconds)

  private val state: ConsensusState[F] =
    NodeViewHolder.StateEval.make[F](system)

  private val statsInterpreter =
    StatsInterpreter.Noop.make[F]

  private def mints(
    etaCalculation:          EtaCalculationAlgebra[F],
    leaderElectionThreshold: LeaderElectionValidationAlgebra[F],
    ed25519VRFResource:      UnsafeResource[F, Ed25519VRF],
    kesProductResource:      UnsafeResource[F, KesProduct],
    ed25519Resource:         UnsafeResource[F, Ed25519]
  ): F[List[BlockMintAlgebra[F]]] =
    stakers
      .parTraverse(staker =>
        for {
          _           <- Logger[F].info(show"Initializing staker key idx=0 address=${staker.address}")
          secureStore <- InMemorySecureStore.Eval.make[F]
          _           <- secureStore.write(UUID.randomUUID().toString, staker.kesKey)
          vrfProofConstruction <- VrfProof.Eval.make[F](
            staker.vrfKey,
            clock,
            leaderElectionThreshold,
            ed25519VRFResource,
            vrfConfig,
            CacheImpl.Eval.make(),
            CacheImpl.Eval.make()
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
            staker.address,
            initialSlot = 0L
          )
          stakerVRFVK <- ed25519VRFResource.use(_.getVerificationKey(staker.vrfKey).pure[F])
          mint =
            BlockMint.Eval.make(
              Staking.Eval.make(
                staker.address,
                LeaderElectionMinting.Eval.make(
                  stakerVRFVK,
                  leaderElectionThreshold,
                  vrfProofConstruction
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
        } yield mint
      )

  private def createBlockStore(
    headerStore: Store[F, BlockHeaderV2],
    bodyStore:   Store[F, BlockBodyV2]
  ): Store[F, BlockV2] =
    new Store[F, BlockV2] {

      def get(id: TypedIdentifier): F[Option[BlockV2]] =
        (OptionT(headerStore.get(id)), OptionT(bodyStore.get(id))).tupled.map((BlockV2.apply _).tupled).value

      def put(t: BlockV2): F[Unit] =
        (headerStore.put(t.headerV2), bodyStore.put(t.blockBodyV2)).tupled.void

      def remove(id: TypedIdentifier): F[Unit] =
        (headerStore.remove(id), bodyStore.remove(id)).tupled.void

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
      blockHeaderStore   <- RefStore.Eval.make[F, BlockHeaderV2]()
      blockBodyStore     <- RefStore.Eval.make[F, BlockBodyV2]()
      blockStore = createBlockStore(blockHeaderStore, blockBodyStore)
      _             <- blockStore.put(genesis)
      slotDataCache <- SlotDataCache.Eval.make(blockHeaderStore, ed25519VRFResource, CacheImpl.Eval.make[SlotData]())
      etaCalculation <- EtaCalculation.Eval.make(
        slotDataCache,
        clock,
        genesis.headerV2.eligibilityCertificate.eta,
        blake2b256Resource,
        blake2b512Resource,
        CacheImpl.Eval.make[Eta]()
      )
      leaderElectionThreshold = LeaderElectionValidation.Eval.make[F](vrfConfig, blake2b512Resource)
      localChain <- LocalChain.Eval.make(
        SlotData(genesis.headerV2)(Ed25519VRF.precomputed()),
        ChainSelection.orderT[F](slotDataCache, blake2b512Resource, ChainSelectionKLookback, ChainSelectionSWindow)
      )
      m <- mints(etaCalculation, leaderElectionThreshold, ed25519VRFResource, kesProductResource, ed25519Resource)
      _ <- EligibilitySimulatorProgram
        .run[F](
          clock,
          m,
          state,
          blockHeaderStore,
          blockStore,
          etaCalculation,
          localChain,
          ed25519VRFResource
        )
    } yield ()
  }
    .guarantee(
      Sync[F].delay(system.terminate()).flatMap(_ => Async[F].fromFuture(system.whenTerminated.pure[F])).void
    )
}

private case class Staker(
  relativeStake: Ratio,
  vrfKey:        SecretKeys.VrfEd25519,
  kesKey:        SecretKeys.KesProduct,
  registration:  Box.Values.TaktikosRegistration,
  address:       TaktikosAddress
)

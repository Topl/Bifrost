package co.topl.demo

import akka.actor.typed.ActorSystem
import akka.util.Timeout
import cats.data.OptionT
import cats.effect.implicits._
import cats.effect.kernel.Sync
import cats.effect.{Async, IO, IOApp}
import cats.implicits._
import co.topl.algebras._
import co.topl.codecs.bytes.implicits._
import co.topl.consensus.LeaderElectionValidation.VrfConfig
import co.topl.consensus._
import co.topl.consensus.algebras.{EtaCalculationAlgebra, LeaderElectionValidationAlgebra}
import co.topl.crypto.hash.blake2b256
import co.topl.crypto.signing.{Ed25519, Ed25519VRF, KesProduct}
import co.topl.minting._
import co.topl.minting.algebras.{BlockMintAlgebra, VrfProofAlgebra}
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility._
import co.topl.typeclasses._
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.nio.file.Files
import java.util.UUID
import scala.concurrent.duration._
import scala.util.Random

object TetraDemo extends IOApp.Simple {

  // Create stubbed/sample/demo data

  private val poolVK =
    new Ed25519().getVerificationKey(KeyInitializer[SecretKeys.Ed25519].random())

  private val stakers = List.fill(5) {

    implicit val ed25519Vrf: Ed25519VRF = Ed25519VRF.precomputed()
    implicit val ed25519: Ed25519 = new Ed25519
    implicit val kesProduct: KesProduct = new KesProduct

    val stakerVrfKey =
      KeyInitializer[SecretKeys.VrfEd25519].random()

    val (kesKey, kesVK) =
      kesProduct.createKeyPair(seed = Bytes(Random.nextBytes(32)), height = (9, 9), 0)

    val stakerRegistration: Box.Values.TaktikosRegistration =
      Box.Values.TaktikosRegistration(
        commitment = kesProduct.sign(
          kesKey,
          Bytes(
            blake2b256
              .hash((ed25519Vrf.getVerificationKey(stakerVrfKey).signableBytes ++ poolVK.bytes.data).toArray)
              .value
          )
        ),
        activationSlot = 0
      )

    val stakerAddress: TaktikosAddress = {
      val paymentKey = KeyInitializer[SecretKeys.Ed25519].random()
      val paymentVerificationKey = ed25519.getVerificationKey(paymentKey)
      TaktikosAddress(
        Sized.strictUnsafe(
          Bytes(blake2b256.hash(paymentVerificationKey.bytes.data.toArray).value)
        ),
        poolVK,
        ed25519.sign(paymentKey, poolVK.bytes.data)
      )
    }
    Staker(Ratio(1, 5), stakerVrfKey, kesKey, stakerRegistration, stakerAddress)
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

  // Interpreter initialization

  type F[A] = IO[A]

  implicit private val logger: Logger[F] = Slf4jLogger.getLogger[F]

  private val vrfConfig =
    VrfConfig(lddCutoff = 40, precision = 16, baselineDifficulty = Ratio(1, 20), amplitude = Ratio(2, 5))

  private val leaderElectionThreshold: LeaderElectionValidationAlgebra[F] =
    LeaderElectionValidation.Eval.make(vrfConfig)

  private val clock: ClockAlgebra[F] =
    AkkaSchedulerClock.Eval.make(100.milli, 720)

  private val vrfProofConstructions: F[List[VrfProofAlgebra[F]]] =
    stakers.traverse(staker =>
      VrfProof.Eval.make[F](
        staker.vrfKey,
        clock,
        leaderElectionThreshold,
        vrfConfig
      )
    )

  implicit private val timeout: Timeout = Timeout(5.seconds)

  private val state: ConsensusState[F] =
    NodeViewHolder.StateEval.make[F](system)

  private def mints(
    etaCalculation:        EtaCalculationAlgebra[F],
    vrfProofConstructions: List[VrfProofAlgebra[F]]
  ): F[List[BlockMintAlgebra[F]]] = {
    val ed25519Vrf = Ed25519VRF.precomputed()
    stakers
      .zip(vrfProofConstructions)
      .traverse { case (staker, vrfProofConstruction) =>
        import staker._
        for {
          _            <- Logger[F].info(show"Initializing staker key idx=0 address=${staker.address}")
          stakerKeyDir <- IO.blocking(Files.createTempDirectory(show"TetraDemoStaker${staker.address}"))
          secureStore  <- AkkaSecureStore.Eval.make[F](stakerKeyDir)
          _            <- secureStore.write(UUID.randomUUID().toString, staker.kesKey)
          _            <- vrfProofConstruction.precomputeForEpoch(0, genesis.headerV2.eligibilityCertificate.eta)
          operationalKeys <- OperationalKeys.FromSecureStore.make[F](
            secureStore = secureStore,
            clock = clock,
            vrfProof = vrfProofConstruction,
            etaCalculation,
            state,
            genesis.headerV2.slotId,
            operationalPeriodLength = 180L,
            activationOperationalPeriod = 0L,
            staker.address
          )
          mint =
            BlockMint.Eval.make(
              Staking.Eval.make(
                staker.address,
                LeaderElectionMinting.Eval.make(
                  ed25519Vrf.getVerificationKey(staker.vrfKey),
                  leaderElectionThreshold,
                  vrfProofConstruction
                ),
                operationalKeys,
                VrfRelativeStakeMintingLookup.Eval.make(state, clock),
                etaCalculation
              ),
              clock
            )
        } yield mint
      }
  }

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

  val run: IO[Unit] = {
    for {
      blockHeaderStore <- RefStore.Eval.make[F, BlockHeaderV2]()
      blockBodyStore   <- RefStore.Eval.make[F, BlockBodyV2]()
      blockStore = createBlockStore(blockHeaderStore, blockBodyStore)
      _             <- blockStore.put(genesis)
      slotDataCache <- SlotDataCache.Eval.make(blockHeaderStore)
      etaCalculation <- EtaCalculation.Eval.make(
        slotDataCache,
        clock,
        genesis.headerV2.eligibilityCertificate.eta
      )
      underlyingHeaderValidation <- BlockHeaderValidation.Eval.make[F](
        etaCalculation,
        VrfRelativeStakeValidationLookup.Eval.make(state, clock),
        leaderElectionThreshold,
        RegistrationLookup.Eval.make(state, clock)
      )
      cachedHeaderValidation <- BlockHeaderValidation.WithCache.make[F](underlyingHeaderValidation, blockHeaderStore)
      localChain <- LocalChain.Eval.make(
        SlotData(genesis.headerV2)(Ed25519VRF.precomputed()),
        ChainSelection.orderT(slotDataCache, 5_000, 200_000)
      )
      constructions <- vrfProofConstructions
      m             <- mints(etaCalculation, constructions)
      _ <- DemoProgram
        .run[F](
          clock,
          m,
          cachedHeaderValidation,
          constructions,
          state,
          blockHeaderStore,
          blockStore,
          etaCalculation,
          localChain
        )
    } yield ()
  }
    .guarantee(
      Sync[F].delay(system.terminate()).flatMap(_ => Async[F].fromFuture(system.whenTerminated.pure[F])).void
    )
}

private case class Staker(
  relativeStake:           Ratio,
  vrfKey:                  SecretKeys.VrfEd25519,
  kesKey:                  SecretKeys.KesProduct,
  registration:            Box.Values.TaktikosRegistration,
  address:                 TaktikosAddress
)(implicit val ed25519VRF: Ed25519VRF, val ed25519: Ed25519, val kesProduct: KesProduct)

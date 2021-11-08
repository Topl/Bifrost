package co.topl.demo

import akka.actor.typed.ActorSystem
import akka.util.Timeout
import cats.data.OptionT
import cats.effect.implicits._
import cats.effect.kernel.Sync
import cats.effect.{Async, IO, IOApp}
import cats.implicits._
import co.topl.algebras._
import co.topl.consensus.LeaderElectionValidation.VrfConfig
import co.topl.consensus._
import co.topl.consensus.algebras.{EtaCalculationAlgebra, LeaderElectionValidationAlgebra}
import co.topl.crypto.hash.blake2b256
import co.topl.crypto.signing.{Ed25519, Ed25519VRF}
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

import scala.concurrent.duration._

object TetraDemo extends IOApp.Simple {

  // Create stubbed/sample/demo data

  private val stakers = List.fill(5) {

    implicit val ed25519Vrf: Ed25519VRF =
      Ed25519VRF.precomputed()

    val stakerVrfKey =
      KeyInitializer[SecretKeys.VrfEd25519].random()

    val stakerRegistration: Box.Values.TaktikosRegistration =
      Box.Values.TaktikosRegistration(
        vrfCommitment = Sized.strictUnsafe(
          Bytes(
            blake2b256
              .hash(stakerVrfKey.verificationKey[VerificationKeys.VrfEd25519].signableBytes.toArray)
              .value
          )
        ),
        extendedVk = VerificationKeys.ExtendedEd25519(
          VerificationKeys.Ed25519(Sized.strictUnsafe(Bytes(Array.fill[Byte](32)(0)))),
          Sized.strictUnsafe(Bytes(Array.fill[Byte](32)(0)))
        ),
        registrationSlot = 0
      )

    implicit val ed25519: Ed25519 =
      new Ed25519

    val stakerAddress: TaktikosAddress = {
      val stakingKey = KeyInitializer[SecretKeys.Ed25519].random()
      val stakingVerificationKey =
        implicitly[ContainsVerificationKey[SecretKeys.Ed25519, VerificationKeys.Ed25519]].verificationKeyOf(stakingKey)
      val paymentKey = KeyInitializer[SecretKeys.Ed25519].random()
      val paymentVerificationKey =
        implicitly[ContainsVerificationKey[SecretKeys.Ed25519, VerificationKeys.Ed25519]].verificationKeyOf(paymentKey)
      TaktikosAddress(
        Sized.strictUnsafe(
          Bytes(blake2b256.hash(paymentVerificationKey.bytes.data.toArray).value)
        ),
        stakingVerificationKey.bytes,
        Sized.strictUnsafe(Bytes(Array.fill[Byte](64)(1)))
      )
    }
    Staker(Ratio(1, 5), stakerVrfKey, stakerRegistration, stakerAddress)
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

  private val leaderElectionThreshold: LeaderElectionValidationAlgebra[F] =
    LeaderElectionValidation.Eval.make(
      VrfConfig(lddCutoff = 0, precision = 16, baselineDifficulty = Ratio(4, 15), amplitude = Ratio(4, 5))
    )

  private val clock: ClockAlgebra[F] =
    AkkaSchedulerClock.Eval.make(100.milli, 150)

  private val vrfProofConstructions: F[List[VrfProofAlgebra[F]]] =
    stakers.traverse(staker =>
      VrfProof.Eval.make[F](
        staker.vrfKey,
        clock
      )
    )

  implicit private val timeout: Timeout = Timeout(5.seconds)

  private val state: ConsensusState[F] =
    NodeViewHolder.StateEval.make[F](system)

  private def mints(
    etaCalculation:        EtaCalculationAlgebra[F],
    vrfProofConstructions: List[VrfProofAlgebra[F]]
  ): List[BlockMintAlgebra[F]] =
    stakers
      .zip(vrfProofConstructions)
      .map { case (staker, vrfProofConstruction) =>
        BlockMint.Eval.make(
          Staking.Eval.make(
            staker.address,
            LeaderElectionMinting.Eval.make(
              staker.vrfKey,
              leaderElectionThreshold,
              vrfProofConstruction
            ),
            KeyEvolver.InMemory.make(
              KeyInitializer[SecretKeys.ExtendedEd25519].random()
            ),
            VrfRelativeStakeMintingLookup.Eval.make(state, clock),
            etaCalculation
          ),
          clock
        )
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
      _ <- DemoProgram
        .run[F](
          clock,
          mints(etaCalculation, constructions),
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

case class Staker(
  relativeStake:           Ratio,
  vrfKey:                  SecretKeys.VrfEd25519,
  registration:            Box.Values.TaktikosRegistration,
  address:                 TaktikosAddress
)(implicit val ed25519VRF: Ed25519VRF, val ed25519: Ed25519)

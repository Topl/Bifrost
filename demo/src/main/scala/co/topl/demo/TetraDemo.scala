package co.topl.demo

import akka.actor.typed.ActorSystem
import akka.util.Timeout
import cats.effect.implicits._
import cats.effect.kernel.{Ref, Sync}
import cats.effect.{Async, IO, IOApp}
import cats.implicits._
import co.topl.algebras._
import co.topl.consensus.LeaderElectionValidation.VrfConfig
import co.topl.consensus._
import co.topl.consensus.algebras.{EtaCalculationAlgebra, LeaderElectionValidationAlgebra}
import co.topl.crypto.hash.blake2b256
import co.topl.crypto.kes.KeyEvolvingSignatureScheme
import co.topl.crypto.signatures.{Ed25519, Ed25519VRF}
import co.topl.crypto.typeclasses._
import co.topl.crypto.typeclasses.implicits._
import co.topl.minting._
import co.topl.minting.algebras.{BlockMintAlgebra, VrfProofAlgebra}
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility._
import co.topl.typeclasses._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration._

object TetraDemo extends IOApp.Simple {

  // Create stubbed/sample/demo data

  private val stakers = List.fill(5) {

    implicit val ed25519Vrf: Ed25519VRF =
      Ed25519VRF.precomputed()

    implicit val ed25519: Ed25519 =
      new Ed25519

    val stakerVrfKey =
      KeyInitializer[SecretKeys.Vrf].random()

    val stakerRegistration: Box.Values.TaktikosRegistration =
      Box.Values.TaktikosRegistration(
        vrfCommitment = Sized.strictUnsafe(
          Bytes(
            blake2b256
              .hash(stakerVrfKey.verificationKey[VerificationKeys.Vrf].signableBytes.toArray)
              .value
          )
        ),
        extendedVk = VerificationKeys.ExtendedEd25519(
          VerificationKeys.Ed25519(Sized.strictUnsafe(Bytes(Array.fill[Byte](32)(0)))),
          Sized.strictUnsafe(Bytes(Array.fill[Byte](32)(0)))
        ),
        registrationSlot = 0
      )

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

  private val initialNodeView =
    NodeView(
      BlockGenesis(Nil).value,
      Map.empty,
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
        implicit val scheme: KeyEvolvingSignatureScheme = new KeyEvolvingSignatureScheme
        import staker.ed25519
        BlockMint.Eval.make(
          Staking.Eval.make(
            staker.address,
            LeaderElectionMinting.Eval.make(
              staker.vrfKey.verificationKey[VerificationKeys.Vrf],
              leaderElectionThreshold,
              vrfProofConstruction
            ),
            KeyEvolver.InMemory.make[F] {
              implicit val slot: Slot = 0
              KeyInitializer[SecretKeys.SymmetricMMM].random()
            },
            VrfRelativeStakeMintingLookup.Eval.make(state, clock),
            etaCalculation
          ),
          clock
        )
      }

  private val blockHeaderLookup: BlockHeaderLookup[F] =
    id => state.lookupBlockHeader(id)

  // Program definition

  val run: IO[Unit] = {
    for {
      slotDataCache <- SlotDataCache.Eval.make(blockHeaderLookup)
      etaCalculation <- EtaCalculation.Eval.make(
        slotDataCache,
        clock,
        initialNodeView.genesisBlock.headerV2.eligibibilityCertificate.eta
      )
      headerValidation <- BlockHeaderValidation.Eval.make[F](
        etaCalculation,
        VrfRelativeStakeValidationLookup.Eval.make(state, clock),
        leaderElectionThreshold,
        RegistrationLookup.Eval.make(state, clock)
      )
      localChain <- LocalChain.Eval.make(
        SlotData(initialNodeView.genesisBlock.headerV2)(Ed25519VRF.precomputed()),
        ChainSelection.orderT(slotDataCache, 5_000, 200_000)
      )
      constructions <- vrfProofConstructions
      _ <- DemoProgram
        .run[F](
          clock,
          mints(etaCalculation, constructions),
          headerValidation,
          constructions,
          state,
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
  vrfKey:                  SecretKeys.Vrf,
  registration:            Box.Values.TaktikosRegistration,
  address:                 TaktikosAddress
)(implicit val ed25519VRF: Ed25519VRF, val ed25519: Ed25519)

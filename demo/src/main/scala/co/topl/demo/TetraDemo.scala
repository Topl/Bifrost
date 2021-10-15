package co.topl.demo

import akka.actor.typed.ActorSystem
import akka.util.Timeout
import cats.effect.kernel.Sync
import cats.effect.{Async, IO, IOApp}
import cats.implicits._
import co.topl.algebras._
import co.topl.consensus.LeaderElectionValidation.VrfConfig
import co.topl.consensus._
import co.topl.consensus.algebras.{BlockHeaderValidationAlgebra, LeaderElectionValidationAlgebra}
import co.topl.crypto.hash.blake2b256
import co.topl.crypto.typeclasses._
import co.topl.crypto.typeclasses.implicits.{extendedEd25519Initializer, _}
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

  private val stakerVrfKey =
    KeyInitializer[SecretKeys.VrfEd25519].random()

  private val stakerRegistration: Box.Values.TaktikosRegistration =
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

  private val stakerAddress: TaktikosAddress = {
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

  private val initialNodeView =
    NodeView(
      BlockGenesis(Nil).value,
      Map.empty,
      Map(0L  -> Map(stakerAddress -> Ratio(9, 10))),
      Map(0L  -> Map(stakerAddress -> stakerRegistration)),
      Map(-1L -> Sized.strictUnsafe[Bytes, Lengths.`32`.type](Bytes(Array.fill[Byte](32)(0))))
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
      VrfConfig(lddCutoff = 0, precision = 16, baselineDifficulty = Ratio(1, 15), amplitude = Ratio(2, 5))
    )

  private val clock: ClockAlgebra[F] =
    AkkaSchedulerClock.Eval.make(100.milli, 150)

  private val vrfProofConstruction: VrfProofAlgebra[F] =
    VrfProof.Eval.make[F](stakerVrfKey, clock)

  implicit private val timeout: Timeout = Timeout(5.seconds)

  private val state: BlockchainState[F] =
    NodeViewHolder.StateEval.make[F](system)

  private val mint: BlockMintAlgebra[F] =
    BlockMint.Eval.make(
      Staking.Eval.make(
        stakerAddress,
        LeaderElectionMinting.Eval.make(
          stakerVrfKey,
          leaderElectionThreshold,
          vrfProofConstruction
        ),
        KeyEvolver.InMemory.make {
          KeyInitializer[SecretKeys.ExtendedEd25519].random()
        },
        VrfRelativeStakeMintingLookup.Eval.make(state, clock),
        EtaMinting.Eval.make(state, clock)
      ),
      clock
    )

  private val headerValidation: BlockHeaderValidationAlgebra[F] =
    BlockHeaderValidation.Eval.Stateful
      .make[F](
        EtaValidation.Eval.make(state, clock),
        VrfRelativeStakeValidationLookup.Eval.make(state, clock),
        leaderElectionThreshold,
        RegistrationLookup.Eval.make(state, clock)
      )

  // Program definition

  val run: IO[Unit] = DemoProgram
    .run[F](clock, mint, headerValidation, vrfProofConstruction, state, EtaCalculation.Eval.make(state, clock))
    .guarantee(
      Sync[F].delay(system.terminate()).flatMap(_ => Async[F].fromFuture(system.whenTerminated.pure[F])).void
    )
}

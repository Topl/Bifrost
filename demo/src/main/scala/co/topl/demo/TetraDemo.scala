package co.topl.demo

import cats._
import cats.implicits._
import cats.tagless.implicits._
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.algebras._
import co.topl.consensus.LeaderElectionValidation.VrfConfig
import co.topl.consensus._
import co.topl.consensus.algebras.{BlockHeaderValidationAlgebra, LeaderElectionValidationAlgebra}
import co.topl.crypto.hash.blake2b256
import co.topl.crypto.typeclasses._
import co.topl.crypto.typeclasses.implicits._
import co.topl.minting._
import co.topl.minting.algebras.{BlockMintAlgebra, LeaderElectionMintingAlgebra}
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility._
import co.topl.typeclasses._

import scala.concurrent.duration._

object TetraDemo extends App {

  type F[A] = Either[Throwable, A]

  implicit def logger[F[_]: Applicative]: LoggerAlgebra[F] =
    new LoggerAlgebra[F] {

      def debug(message: String): F[Unit] =
        println("[DEBUG] " + message).pure[F]

      def info(message: String): F[Unit] =
        println("[INFO] " + message).pure[F]
    }

  val stakerRelativeStake =
    Ratio(9, 10)

  val leaderElectionThreshold: LeaderElectionValidationAlgebra[F] =
    LeaderElectionValidation.Eval.make(
      VrfConfig(lddCutoff = 0, precision = 16, baselineDifficulty = Ratio(1, 15), amplitude = Ratio(2, 5))
    )

  private val stakerVrfKey =
    KeyInitializer[SecretKeys.Vrf].random()

  // TODO
  val stakerRegistration: Box.Values.TaktikosRegistration =
    Box.Values.TaktikosRegistration(
      vrfCommitment = Sized.strictUnsafe(
        Bytes(blake2b256.hash(stakerVrfKey.verificationKey[VerificationKeys.Vrf].signableBytes.toArray).value)
      ),
      extendedVk = VerificationKeys.ExtendedEd25519(
        VerificationKeys.Ed25519(Sized.strictUnsafe(Bytes(Array.fill[Byte](32)(0)))),
        Sized.strictUnsafe(Bytes(Array.fill[Byte](32)(0)))
      ),
      registrationSlot = 0
    )

  val clock: ClockAlgebra[F] =
    new SyncClockInterpreter[F](100.milli, 150)

  val vrfProof = VrfProof.Eval.make[F](stakerVrfKey, clock)

  val leaderElectionHit: LeaderElectionMintingAlgebra[F] = LeaderElectionMinting.Eval.make(
    stakerVrfKey,
    leaderElectionThreshold,
    vrfProof
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

  val state = InMemoryState.Eval.make[F](
    BlockGenesis(Nil).value,
    initialRelativeStakes = Map(0L -> Map(stakerAddress -> stakerRelativeStake)),
    initialRegistrations = Map(0L -> Map(stakerAddress -> stakerRegistration)),
    initialEtas = Map(-1L -> Sized.strictUnsafe[Bytes, Lengths.`32`.type](Bytes(Array.fill[Byte](32)(0))))
  )

  val mint: BlockMintAlgebra[F] =
    BlockMint.Eval.make(
      Staking.Eval.make(
        stakerAddress,
        leaderElectionHit,
        BlockSigning.Eval.make(
          KeyEvolver.InMemory.make {
            implicit val slot: Slot = 0
            KeyInitializer[SecretKeys.SymmetricMMM].random()
          }
        ),
        VrfRelativeStakeMintingLookup.Eval.make(state, clock),
        EtaMinting.Eval.make(state, clock)
      ),
      clock
    )

  val headerValidation: BlockHeaderValidationAlgebra[F] =
    BlockHeaderValidation.Eval.Stateful
      .make[F](
        EtaValidation.Eval.make(state, clock),
        VrfRelativeStakeValidationLookup.Eval.make(state, clock),
        leaderElectionThreshold,
        RegistrationLookup.Eval.make(state, clock)
      )

  DemoProgram
    .run[F](clock, mint, headerValidation, vrfProof, state, EtaCalculation.Eval.make(state, clock))
    .onError { case e =>
      throw e
    }

}

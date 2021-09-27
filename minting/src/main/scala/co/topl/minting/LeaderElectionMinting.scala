package co.topl.minting

import cats.Monad
import cats.data.OptionT
import cats.implicits._
import co.topl.consensus.LeaderElectionValidation
import co.topl.consensus.algebras.LeaderElectionValidationAlgebra
import co.topl.consensus.vrf.ProofToHash
import co.topl.crypto.signatures.Ed25519VRF
import co.topl.crypto.typeclasses.implicits._
import co.topl.minting.algebras.LeaderElectionMintingAlgebra
import co.topl.minting.algebras.LeaderElectionMintingAlgebra.VrfHit
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.{Ratio, Sized}
import co.topl.typeclasses.implicits._

object LeaderElectionMinting {

  object Eval {

    // TODO: A Cache of (Test Proof, Test Proof Hashed)
    // Cache is computed using co.topl.algebras.ClockAlgebra.Implicits.ClockOps.epochRange.map(slot => calculateTestProof(slot))

    def make[F[_]: Monad](
      secret:               SecretKeys.Vrf,
      thresholdInterpreter: LeaderElectionValidationAlgebra[F]
    ): LeaderElectionMintingAlgebra[F] = new LeaderElectionMintingAlgebra[F] {

      private def buildHit(slot: Slot, eta: Eta, testProof: Proofs.Signature.VrfEd25519, threshold: Ratio): VrfHit =
        VrfHit(
          EligibilityCertificate(
            VrfProof(
              secret,
              LeaderElectionValidation.VrfArgument(eta, slot, LeaderElectionValidation.Tokens.Nonce)
            ),
            testProof,
            secret.verificationKey[VerificationKeys.Vrf],
            threshold.evidence,
            eta
          ),
          slot,
          threshold
        )

      def getHit(relativeStake: Ratio, slot: Slot, slotDiff: Epoch, eta: Eta): F[Option[VrfHit]] =
        thresholdInterpreter
          .getThreshold(relativeStake, slotDiff)
          .flatMap { threshold =>
            val testProof =
              VrfProof(secret, LeaderElectionValidation.VrfArgument(eta, slot, LeaderElectionValidation.Tokens.Test))
            thresholdInterpreter
              .isSlotLeaderForThreshold(threshold)(ProofToHash.digest(testProof))
              .map(Option.when(_)(buildHit(slot, eta, testProof, threshold)))
          }
    }

    object VrfProof {

      def apply(skVrf: SecretKeys.Vrf, arg: LeaderElectionValidation.VrfArgument): Proofs.Signature.VrfEd25519 =
        Proofs.Signature.VrfEd25519(
          Sized.strictUnsafe(
            Bytes(
              Ed25519VRF.instance.vrfProof(
                skVrf.ed25519.bytes.data.toArray,
                arg.signableBytes.toArray
              )
            )
          )
        )
    }
  }
}

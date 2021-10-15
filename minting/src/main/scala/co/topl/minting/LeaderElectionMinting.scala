package co.topl.minting

import cats.Monad
import cats.implicits._
import co.topl.consensus.algebras.LeaderElectionValidationAlgebra
import co.topl.minting.algebras.LeaderElectionMintingAlgebra.VrfHit
import co.topl.minting.algebras.{LeaderElectionMintingAlgebra, VrfProofAlgebra}
import co.topl.models._
import co.topl.models.utility.Ratio
import co.topl.typeclasses.implicits._

object LeaderElectionMinting {

  object Eval {

    def make[F[_]: Monad](
      secret:               SecretKeys.VrfEd25519,
      thresholdInterpreter: LeaderElectionValidationAlgebra[F],
      vrfProofAlgebra:      VrfProofAlgebra[F]
    ): LeaderElectionMintingAlgebra[F] = new LeaderElectionMintingAlgebra[F] {

      private def buildHit(slot: Slot, eta: Eta, testProof: Proofs.Signature.VrfEd25519, threshold: Ratio): F[VrfHit] =
        vrfProofAlgebra
          .nonceProofForSlot(slot, eta)
          .map(nonceProof =>
            VrfHit(
              EligibilityCertificate(
                nonceProof,
                testProof,
                secret.verificationKey[VerificationKeys.VrfEd25519],
                threshold.evidence,
                eta
              ),
              slot,
              threshold
            )
          )

      def getHit(relativeStake: Ratio, slot: Slot, slotDiff: Epoch, eta: Eta): F[Option[VrfHit]] =
        (
          thresholdInterpreter.getThreshold(relativeStake, slotDiff),
          vrfProofAlgebra.testProofForSlot(slot, eta),
          vrfProofAlgebra.rhoForSlot(slot, eta)
        ).tupled
          .flatMap { case (threshold, testProof, rho) =>
            thresholdInterpreter
              .isSlotLeaderForThreshold(threshold)(rho)
              .ifA(
                buildHit(slot, eta, testProof, threshold).map(_.some),
                none[VrfHit].pure[F]
              )
          }
    }
  }
}

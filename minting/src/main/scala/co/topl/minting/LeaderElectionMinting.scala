package co.topl.minting

import cats.Monad
import cats.implicits._
import co.topl.consensus.LeaderElectionValidation
import co.topl.consensus.algebras.LeaderElectionValidationAlgebra
import co.topl.consensus.vrf.ProofToHash
import co.topl.crypto.signatures.Ed25519VRF
import co.topl.crypto.typeclasses.implicits._
import co.topl.minting.algebras.LeaderElectionMintingAlgebra
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.{Ratio, Sized}

object LeaderElectionMinting {

  object Eval {

    def make[F[_]: Monad](
      secret:               SecretKeys.Vrf,
      thresholdInterpreter: LeaderElectionValidationAlgebra[F]
    ): LeaderElectionMintingAlgebra[F] =
      (relativeStake: Ratio, slot: Slot, slotDiff: Epoch, eta: Eta) =>
        thresholdInterpreter
          .getThreshold(relativeStake, slotDiff)
          .flatMap { threshold =>
            val testProof = VrfProof.test(secret, eta, slot)
            thresholdInterpreter
              .isSlotLeaderForThreshold(threshold)(ProofToHash.digest(testProof))
              .map(isSlotLeader =>
                if (isSlotLeader)
                  Vrf
                    .Hit(
                      Vrf.Certificate(
                        secret.verificationKey[VerificationKeys.Vrf],
                        VrfProof.nonce(secret, eta, slot),
                        testProof
                      ),
                      slot,
                      threshold
                    )
                    .some
                else
                  none[Vrf.Hit]
              )
          }

    object VrfProof {

      private def proofBytes(
        skVrf: SecretKeys.Vrf,
        arg:   LeaderElectionValidation.VrfArgument
      ): Bytes =
        Bytes(
          Ed25519VRF.instance.vrfProof(
            skVrf.ed25519.bytes.data.toArray,
            arg.signableBytes.toArray
          )
        )

      def test(skVrf: SecretKeys.Vrf, eta: Eta, slot: Slot): Proofs.Vrf.Test =
        Proofs.Vrf.Test(
          Sized.strictUnsafe(
            proofBytes(skVrf, LeaderElectionValidation.VrfArgument(eta, slot, LeaderElectionValidation.Tokens.Test))
          )
        )

      def nonce(skVrf: SecretKeys.Vrf, eta: Eta, slot: Slot): Proofs.Vrf.Nonce =
        Proofs.Vrf.Nonce(
          Sized.strictUnsafe(
            proofBytes(skVrf, LeaderElectionValidation.VrfArgument(eta, slot, LeaderElectionValidation.Tokens.Nonce))
          )
        )
    }
  }
}

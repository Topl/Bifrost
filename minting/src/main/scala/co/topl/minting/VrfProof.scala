package co.topl.minting

import cats.Monad
import cats.effect.Sync
import cats.implicits._
import co.topl.algebras.ClockAlgebra
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.consensus.LeaderElectionValidation
import co.topl.consensus.vrf.ProofToHash
import co.topl.crypto.signing.Ed25519VRF
import co.topl.minting.algebras.VrfProofAlgebra
import co.topl.models.Proofs.Signature
import co.topl.models._
import co.topl.typeclasses.implicits._

import scala.collection.concurrent.TrieMap
import scala.collection.immutable.LongMap

object VrfProof {

  object Eval {

    def make[F[_]: Monad: Sync](skVrf: SecretKeys.VrfEd25519, clock: ClockAlgebra[F]): VrfProofAlgebra[F] =
      new VrfProofAlgebra[F] {

        private val testProofs: TrieMap[Eta, LongMap[Signature.VrfEd25519]] = TrieMap.empty
        private val rhos: TrieMap[Eta, LongMap[Rho]] = TrieMap.empty

        def precomputeForEpoch(epoch: Epoch, previousEta: Eta): F[Unit] =
          Sync[F]
            .defer(clock.epochRange(epoch))
            .flatMap(boundary =>
              Sync[F].delay(
                LongMap.from(
                  boundary.map { slot =>
                    slot -> compute(
                      skVrf,
                      LeaderElectionValidation.VrfArgument(previousEta, slot, LeaderElectionValidation.Tokens.Test)
                    )
                  }
                )
              )
            )
            .map(testProofsForEta =>
              testProofsForEta -> LongMap.from(testProofsForEta.view.mapValues(ProofToHash.digest))
            )
            .flatTap { case (testProofsForEta, rhosForEta) =>
              Sync[F].delay {
                testProofs.addOne(previousEta -> testProofsForEta)
                rhos.addOne(previousEta       -> rhosForEta)
              }
            }
            .void

        def testProofForSlot(slot: Slot, eta: Eta): F[Signature.VrfEd25519] =
          Sync[F].delay(testProofs(eta)(slot))

        def nonceProofForSlot(slot: Slot, eta: Eta): F[Signature.VrfEd25519] =
          Sync[F].delay(
            compute(skVrf, LeaderElectionValidation.VrfArgument(eta, slot, LeaderElectionValidation.Tokens.Nonce))
          )

        def rhoForSlot(slot: Slot, eta: Eta): F[Rho] =
          Sync[F].delay(rhos(eta)(slot))
      }
  }

  private def compute(
    skVrf: SecretKeys.VrfEd25519,
    arg:   LeaderElectionValidation.VrfArgument
  ): Proofs.Signature.VrfEd25519 = Ed25519VRF.instance.sign(skVrf, arg.signableBytes)

}

package co.topl.minting

import cats.effect.Sync
import cats.implicits._
import cats.{Applicative, Monad}
import co.topl.algebras.ClockAlgebra
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.consensus.LeaderElectionValidation
import co.topl.consensus.LeaderElectionValidation.signableVrfArgument
import co.topl.consensus.vrf.ProofToHash
import co.topl.crypto.signing.Ed25519VRF
import co.topl.crypto.typeclasses.implicits._
import co.topl.minting.algebras.VrfProofAlgebra
import co.topl.models.Proofs.Signature
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.Sized

import scala.collection.concurrent.TrieMap
import scala.collection.immutable.LongMap

object VrfProof {

  object Eval {

    def make[F[_]: Monad: Sync](skVrf: SecretKeys.Vrf, clock: ClockAlgebra[F]): VrfProofAlgebra[F] =
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

  private def compute(skVrf: SecretKeys.Vrf, arg: LeaderElectionValidation.VrfArgument): Proofs.Signature.VrfEd25519 =
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

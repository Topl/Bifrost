package co.topl.minting

import cats.MonadError
import cats.data.OptionT
import cats.effect.{Ref, Sync}
import cats.implicits._
import co.topl.algebras.ClockAlgebra
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.consensus.LeaderElectionValidation
import co.topl.consensus.LeaderElectionValidation.signableVrfArgument
import co.topl.consensus.vrf.ProofToHash
import co.topl.crypto.signatures.Ed25519VRF
import co.topl.crypto.typeclasses.implicits._
import co.topl.minting.algebras.VrfProofAlgebra
import co.topl.models.Proofs.Signature
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.Sized
import co.topl.typeclasses.implicits._
import scalacache.caffeine.CaffeineCache
import scalacache.{CacheConfig, CacheKeyBuilder}

import scala.collection.immutable.LongMap

object VrfProof {

  object Eval {

    def make[F[_]: MonadError[*[_], Throwable]: Sync](
      skVrf: SecretKeys.Vrf,
      clock: ClockAlgebra[F]
    ): F[VrfProofAlgebra[F]] = {
      implicit val cacheConfig: CacheConfig = CacheConfig(cacheKeyBuilder = new CacheKeyBuilder {
        def toCacheKey(parts: Seq[Any]): String =
          parts.map {
            case eta: Eta => eta.data.show
            case _        => throw new MatchError()
          }.mkString

        def stringToCacheKey(key: String): String = key
      })

      CaffeineCache[F, LongMap[Signature.VrfEd25519]].flatMap { implicit testProofs =>
        CaffeineCache[F, LongMap[Rho]].flatMap { implicit rhos =>
          Ref
            .of[F, Ed25519VRF](Ed25519VRF.precomputed())
            .map(ed25519VRFRef =>
              new VrfProofAlgebra[F] {

                def precomputeForEpoch(epoch: Epoch, eta: Eta): F[Unit] =
                  clock
                    .epochRange(epoch)
                    .flatMap(boundary =>
                      ed25519VRFRef.modify { implicit ed =>
                        val testProofs = LongMap.from(
                          boundary.map { slot =>
                            slot -> compute(
                              LeaderElectionValidation
                                .VrfArgument(eta, slot, LeaderElectionValidation.Tokens.Test),
                              ed
                            )
                          }
                        )
                        val rhoValues = LongMap.from(testProofs.view.mapValues(ProofToHash.digest))
                        ed -> (testProofs -> rhoValues)
                      }
                    )
                    .flatTap { case (testProofsForEta, rhosForEta) =>
                      (testProofs.put(eta)(testProofsForEta), rhos.put(eta)(rhosForEta)).tupled
                    }
                    .void

                def testProofForSlot(slot: Slot, eta: Eta): F[Signature.VrfEd25519] =
                  OptionT(testProofs.get(eta))
                    .subflatMap(_.get(slot))
                    .getOrElseF(
                      new IllegalStateException(show"testProof was not precomputed for slot=$slot eta=$eta")
                        .raiseError[F, Signature.VrfEd25519]
                    )

                def nonceProofForSlot(slot: Slot, eta: Eta): F[Signature.VrfEd25519] =
                  ed25519VRFRef.modify(ed =>
                    ed -> compute(
                      LeaderElectionValidation.VrfArgument(eta, slot, LeaderElectionValidation.Tokens.Nonce),
                      ed
                    )
                  )

                def rhoForSlot(slot: Slot, eta: Eta): F[Rho] =
                  OptionT(rhos.get(eta))
                    .subflatMap(_.get(slot))
                    .getOrElseF(
                      new IllegalStateException(show"rho was not precomputed for slot=$slot eta=$eta")
                        .raiseError[F, Rho]
                    )

                private def compute(
                  arg:        LeaderElectionValidation.VrfArgument,
                  ed25519VRF: Ed25519VRF
                ): Proofs.Signature.VrfEd25519 =
                  Proofs.Signature.VrfEd25519(
                    Sized.strictUnsafe(
                      Bytes(
                        ed25519VRF.vrfProof(
                          skVrf.ed25519.bytes.data.toArray,
                          arg.signableBytes.toArray
                        )
                      )
                    )
                  )
              }
            )
        }
      }
    }
  }
}

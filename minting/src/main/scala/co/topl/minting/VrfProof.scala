package co.topl.minting

import cats.MonadError
import cats.data.OptionT
import cats.effect.{Ref, Sync}
import cats.implicits._
import co.topl.algebras.ClockAlgebra
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.consensus.LeaderElectionValidation
import co.topl.crypto.signing.Ed25519VRF
import co.topl.minting.algebras.VrfProofAlgebra
import co.topl.models.Proofs.Knowledge
import co.topl.models._
import co.topl.typeclasses.implicits._
import scalacache.caffeine.CaffeineCache
import scalacache.{CacheConfig, CacheKeyBuilder}

import scala.collection.immutable.LongMap

object VrfProof {

  object Eval {

    def make[F[_]: MonadError[*[_], Throwable]: Sync](
      skVrf: SecretKeys.VrfEd25519,
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

      CaffeineCache[F, LongMap[Knowledge.VrfEd25519]].flatMap { implicit testProofs =>
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
                        val rhoValues = LongMap.from(testProofs.view.mapValues(ed.proofToHash))
                        ed -> (testProofs -> rhoValues)
                      }
                    )
                    .flatTap { case (testProofsForEta, rhosForEta) =>
                      (testProofs.put(eta)(testProofsForEta), rhos.put(eta)(rhosForEta)).tupled
                    }
                    .void

                def testProofForSlot(slot: Slot, eta: Eta): F[Knowledge.VrfEd25519] =
                  OptionT(testProofs.get(eta))
                    .subflatMap(_.get(slot))
                    .getOrElseF(
                      new IllegalStateException(show"testProof was not precomputed for slot=$slot eta=$eta")
                        .raiseError[F, Knowledge.VrfEd25519]
                    )

                def nonceProofForSlot(slot: Slot, eta: Eta): F[Knowledge.VrfEd25519] =
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
                ): Proofs.Knowledge.VrfEd25519 =
                  ed25519VRF.sign(
                    skVrf,
                    arg.signableBytes
                  )
              }
            )
        }
      }
    }
  }
}

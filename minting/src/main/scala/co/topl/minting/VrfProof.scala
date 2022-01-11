package co.topl.minting

import cats.MonadError
import cats.data.OptionT
import cats.effect.{Ref, Sync}
import cats.implicits._
import co.topl.algebras.ClockAlgebra
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.consensus.LeaderElectionValidation
import co.topl.consensus.LeaderElectionValidation.VrfConfig
import co.topl.consensus.algebras.LeaderElectionValidationAlgebra
import co.topl.crypto.signing.Ed25519VRF
import co.topl.minting.algebras.VrfProofAlgebra
import co.topl.models._
import co.topl.models.utility.Ratio
import co.topl.typeclasses.implicits._
import scalacache.caffeine.CaffeineCache
import scalacache.{CacheConfig, CacheKeyBuilder}

import scala.collection.immutable.{LongMap, NumericRange}

object VrfProof {

  object Eval {

    def make[F[_]: MonadError[*[_], Throwable]: Sync](
      skVrf:                    SecretKeys.VrfEd25519,
      clock:                    ClockAlgebra[F],
      leaderElectionValidation: LeaderElectionValidationAlgebra[F],
      vrfConfig:                VrfConfig
    ): F[VrfProofAlgebra[F]] = {
      implicit val cacheConfig: CacheConfig = CacheConfig(cacheKeyBuilder = new CacheKeyBuilder {
        def toCacheKey(parts: Seq[Any]): String =
          parts.map {
            case eta: Eta @unchecked => eta.data.show
            case t                   => throw new MatchError(t)
          }.mkString

        def stringToCacheKey(key: String): String = key
      })

      CaffeineCache[F, LongMap[Proofs.Knowledge.VrfEd25519]].flatMap { implicit vrfProofsCache =>
        CaffeineCache[F, LongMap[Rho]].flatMap { implicit rhosCache =>
          Ref
            .of[F, Ed25519VRF](Ed25519VRF.precomputed())
            .map(ed25519VRFRef =>
              new VrfProofAlgebra[F] {

                def precomputeForEpoch(epoch: Epoch, eta: Eta): F[Unit] =
                  clock
                    .epochRange(epoch)
                    .flatMap(boundary =>
                      ed25519VRFRef.modify { implicit ed =>
                        val vrfProofs = LongMap.from(
                          boundary.map { slot =>
                            slot -> compute(
                              LeaderElectionValidation.VrfArgument(eta, slot),
                              ed
                            )
                          }
                        )
                        val rhoValues = LongMap.from(vrfProofs.view.mapValues(ed.proofToHash))
                        ed -> (vrfProofs -> rhoValues)
                      }
                    )
                    .flatTap { case (vrfProofs, rhoValues) =>
                      (vrfProofsCache.put(eta)(vrfProofs), rhosCache.put(eta)(rhoValues)).tupled
                    }
                    .void

                def proofForSlot(slot: Slot, eta: Eta): F[Proofs.Knowledge.VrfEd25519] =
                  OptionT(vrfProofsCache.get(eta))
                    .subflatMap(_.get(slot))
                    .getOrElseF(
                      new IllegalStateException(show"testProof was not precomputed for slot=$slot eta=$eta")
                        .raiseError[F, Proofs.Knowledge.VrfEd25519]
                    )

                def rhoForSlot(slot: Slot, eta: Eta): F[Rho] =
                  OptionT(rhosCache.get(eta))
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

                def ineligibleSlots(
                  epoch:         Epoch,
                  eta:           Eta,
                  inRange:       Option[NumericRange.Exclusive[Long]],
                  relativeStake: Ratio
                ): F[Vector[Slot]] =
                  for {
                    rhosMap <-
                      OptionT(rhosCache.get(eta))
                        .getOrElseF(
                          new IllegalStateException(show"rhos were not precomputed for epoch=$epoch eta=$eta")
                            .raiseError[F, LongMap[Rho]]
                        )
                    rhosList = rhosMap.toList
                    rhos = inRange.fold(rhosList)(r => rhosList.filter(l1 => r.contains(l1._1)))
                    threshold <- leaderElectionValidation
                      .getThreshold(relativeStake, vrfConfig.lddCutoff)
                    leaderCalculations <- rhos.traverse { case (slot, rho) =>
                      leaderElectionValidation
                        .isSlotLeaderForThreshold(threshold)(rho)
                        .map(isLeader => slot -> isLeader)
                    }
                    slots = leaderCalculations.collect { case (slot, false) => slot }.toVector
                  } yield slots
              }
            )
        }
      }
    }
  }
}

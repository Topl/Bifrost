package co.topl.minting

import cats.{MonadError, Parallel}
import cats.data.OptionT
import cats.effect.Sync
import cats.implicits._
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.algebras.{ClockAlgebra, UnsafeResource}
import co.topl.consensus.LeaderElectionValidation
import co.topl.consensus.LeaderElectionValidation.VrfConfig
import co.topl.consensus.algebras.LeaderElectionValidationAlgebra
import co.topl.crypto.signing.Ed25519VRF
import co.topl.minting.algebras.VrfProofAlgebra
import co.topl.models._
import co.topl.models.utility.Ratio
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger
import scalacache.caffeine.CaffeineCache
import scalacache.{CacheConfig, CacheKeyBuilder}

import scala.collection.immutable.{LongMap, NumericRange}

object VrfProof {

  object Eval {

    implicit private val cacheConfig: CacheConfig = CacheConfig(cacheKeyBuilder = new CacheKeyBuilder {

      def toCacheKey(parts: Seq[Any]): String =
        parts.map {
          case eta: Eta @unchecked => eta.data.show
          case t                   => throw new MatchError(t)
        }.mkString

      def stringToCacheKey(key: String): String = key
    })

    def make[F[_]: MonadError[*[_], Throwable]: Sync: Logger: Parallel](
      skVrf:                    SecretKeys.VrfEd25519,
      clock:                    ClockAlgebra[F],
      leaderElectionValidation: LeaderElectionValidationAlgebra[F],
      ed25519VRFResource:       UnsafeResource[F, Ed25519VRF],
      vrfConfig:                VrfConfig
    ): F[VrfProofAlgebra[F]] =
      (CaffeineCache[F, LongMap[Proofs.Knowledge.VrfEd25519]], CaffeineCache[F, LongMap[Rho]]).mapN(
        (vrfProofsCache, rhosCache) =>
          new Impl[F](
            skVrf,
            clock,
            leaderElectionValidation,
            ed25519VRFResource,
            vrfConfig,
            vrfProofsCache,
            rhosCache
          )
      )

    private class Impl[F[_]: MonadError[*[_], Throwable]: Sync: Logger: Parallel](
      skVrf:                    SecretKeys.VrfEd25519,
      clock:                    ClockAlgebra[F],
      leaderElectionValidation: LeaderElectionValidationAlgebra[F],
      ed25519VRFResource:       UnsafeResource[F, Ed25519VRF],
      vrfConfig:                VrfConfig,
      vrfProofsCache:           CaffeineCache[F, LongMap[Proofs.Knowledge.VrfEd25519]],
      rhosCache:                CaffeineCache[F, LongMap[Rho]]
    ) extends VrfProofAlgebra[F] {

      def precomputeForEpoch(epoch: Epoch, eta: Eta): F[Unit] =
        for {
          _        <- Logger[F].info(show"Precomputing VRF Proofs and Rho values for epoch=$epoch eta=$eta")
          boundary <- clock.epochRange(epoch)
          (vrfProofs, rhoValues) <- ed25519VRFResource.use { implicit ed =>
            val vrfProofs = LongMap.from(
              boundary.map { slot =>
                slot -> compute(
                  LeaderElectionValidation.VrfArgument(eta, slot),
                  ed
                )
              }
            )
            val rhoValues = LongMap.from(vrfProofs.view.mapValues(ed.proofToHash))
            (vrfProofs -> rhoValues).pure[F]
          }
          _ <- (vrfProofsCache.put(eta)(vrfProofs), rhosCache.put(eta)(rhoValues)).tupled
        } yield ()

      def proofForSlot(slot: Slot, eta: Eta): F[Proofs.Knowledge.VrfEd25519] =
        OptionT(vrfProofsCache.get(eta))
          .subflatMap(_.get(slot))
          .getOrElseF(
            new IllegalStateException(show"proof was not precomputed for slot=$slot eta=$eta")
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
          threshold <- leaderElectionValidation.getThreshold(relativeStake, vrfConfig.lddCutoff)
          leaderCalculations <- rhos.parTraverse { case (slot, rho) =>
            leaderElectionValidation
              .isSlotLeaderForThreshold(threshold)(rho)
              .map(isLeader => slot -> isLeader)
          }
          slots = leaderCalculations.collect { case (slot, false) => slot }.toVector
        } yield slots
    }
  }
}

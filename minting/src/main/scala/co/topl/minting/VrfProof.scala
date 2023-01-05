package co.topl.minting

import cats.data.OptionT
import cats.effect.Sync
import cats.implicits._
import cats.Parallel
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.algebras.{ClockAlgebra, UnsafeResource}
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.consensus.interpreters.LeaderElectionValidation.VrfConfig
import co.topl.consensus.algebras.LeaderElectionValidationAlgebra
import co.topl.consensus.interpreters.LeaderElectionValidation
import co.topl.crypto.signing.Ed25519VRF
import co.topl.minting.algebras.VrfProofAlgebra
import co.topl.models._
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.{Ratio, Sized}
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger
import scalacache.caffeine.CaffeineCache

import scala.collection.immutable.NumericRange

object VrfProof {

  object Eval {

    def make[F[_]: Sync: Logger: Parallel](
      skVrf:                    SecretKeys.VrfEd25519,
      clock:                    ClockAlgebra[F],
      leaderElectionValidation: LeaderElectionValidationAlgebra[F],
      ed25519VRFResource:       UnsafeResource[F, Ed25519VRF],
      vrfConfig:                VrfConfig
    ): F[VrfProofAlgebra[F]] =
      (CaffeineCache[F, Bytes, Map[Long, Proofs.Knowledge.VrfEd25519]], CaffeineCache[F, Bytes, Map[Long, Rho]]).mapN(
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

    private class Impl[F[_]: Sync: Logger: Parallel](
      skVrf:                    SecretKeys.VrfEd25519,
      clock:                    ClockAlgebra[F],
      leaderElectionValidation: LeaderElectionValidationAlgebra[F],
      ed25519VRFResource:       UnsafeResource[F, Ed25519VRF],
      vrfConfig:                VrfConfig,
      vrfProofsCache:           CaffeineCache[F, Bytes, Map[Long, Proofs.Knowledge.VrfEd25519]],
      rhosCache:                CaffeineCache[F, Bytes, Map[Long, Rho]]
    ) extends VrfProofAlgebra[F] {

      def precomputeForEpoch(epoch: Epoch, eta: Eta): F[Unit] =
        for {
          _        <- Logger[F].info(show"Precomputing VRF Proofs and Rho values for epoch=$epoch eta=$eta")
          boundary <- clock.epochRange(epoch)
          (vrfProofs, rhoValues) <- ed25519VRFResource.use { implicit ed =>
            val vrfProofs =
              boundary.map { slot =>
                slot -> compute(
                  LeaderElectionValidation.VrfArgument(eta, slot),
                  ed
                )
              }.toMap

            val rhoValues = vrfProofs.map { case (slot, proof) =>
              slot -> Rho(Sized.strictUnsafe(ed.proofToHash(proof.bytes.data)))
            }
            (vrfProofs -> rhoValues).pure[F]
          }
          _ <- (vrfProofsCache.put(eta.data)(vrfProofs), rhosCache.put(eta.data)(rhoValues)).tupled
        } yield ()

      def proofForSlot(slot: Slot, eta: Eta): F[Proofs.Knowledge.VrfEd25519] =
        OptionT(vrfProofsCache.get(eta.data))
          .orElseF(
            clock.epochOf(slot).flatMap(precomputeForEpoch(_, eta)) >>
            vrfProofsCache.get(eta.data)
          )
          .subflatMap(_.get(slot))
          .getOrElseF(
            new IllegalStateException(show"proof was not precomputed for slot=$slot eta=$eta")
              .raiseError[F, Proofs.Knowledge.VrfEd25519]
          )

      def rhoForSlot(slot: Slot, eta: Eta): F[Rho] =
        OptionT(rhosCache.get(eta.data))
          .orElseF(
            clock.epochOf(slot).flatMap(precomputeForEpoch(_, eta)) >>
            rhosCache.get(eta.data)
          )
          .subflatMap(_.get(slot))
          .getOrElseF(
            new IllegalStateException(show"rho was not precomputed for slot=$slot eta=$eta")
              .raiseError[F, Rho]
          )

      private def compute(
        arg:        LeaderElectionValidation.VrfArgument,
        ed25519VRF: Ed25519VRF
      ): Proofs.Knowledge.VrfEd25519 =
        Proofs.Knowledge.VrfEd25519(
          Sized.strictUnsafe(
            ed25519VRF.sign(
              skVrf.bytes.data,
              arg.signableBytes
            )
          )
        )

      def ineligibleSlots(
        epoch:         Epoch,
        eta:           Eta,
        inRange:       Option[NumericRange.Exclusive[Long]],
        relativeStake: Ratio
      ): F[Vector[Slot]] =
        for {
          rhosMap <-
            OptionT(rhosCache.get(eta.data))
              .orElseF(precomputeForEpoch(epoch, eta) >> rhosCache.get(eta.data))
              .getOrElseF(
                new IllegalStateException(show"rhos were not precomputed for epoch=$epoch eta=$eta")
                  .raiseError[F, Map[Long, Rho]]
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

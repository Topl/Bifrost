package co.topl.minting.interpreters

import cats.Parallel
import cats.data.OptionT
import cats.effect.Sync
import cats.implicits._
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.algebras.{ClockAlgebra, UnsafeResource}
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.consensus.algebras.LeaderElectionValidationAlgebra
import co.topl.consensus.models.{VrfArgument, VrfConfig}
import VrfArgument._
import co.topl.crypto.signing.Ed25519VRF
import co.topl.minting.algebras.VrfCalculatorAlgebra
import co.topl.minting.models.VrfHit
import co.topl.models._
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.{Ratio, Sized}
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger
import scalacache.caffeine.CaffeineCache

import scala.collection.immutable.NumericRange

object VrfCalculator {

  def make[F[_]: Sync: Logger: Parallel](
    vkVrf:                    VerificationKeys.VrfEd25519,
    skVrf:                    SecretKeys.VrfEd25519,
    clock:                    ClockAlgebra[F],
    leaderElectionValidation: LeaderElectionValidationAlgebra[F],
    ed25519VRFResource:       UnsafeResource[F, Ed25519VRF],
    vrfConfig:                VrfConfig,
    thresholdInterpreter:     LeaderElectionValidationAlgebra[F]
  ): F[VrfCalculatorAlgebra[F]] =
    (CaffeineCache[F, Bytes, Map[Long, Proofs.Knowledge.VrfEd25519]], CaffeineCache[F, Bytes, Map[Long, Rho]]).mapN(
      (vrfProofsCache, rhosCache) =>
        new Impl[F](
          vkVrf,
          skVrf,
          clock,
          leaderElectionValidation,
          ed25519VRFResource,
          vrfConfig,
          thresholdInterpreter,
          vrfProofsCache,
          rhosCache
        )
    )

  private class Impl[F[_]: Sync: Logger: Parallel](
    vkVrf:                    VerificationKeys.VrfEd25519,
    skVrf:                    SecretKeys.VrfEd25519,
    clock:                    ClockAlgebra[F],
    leaderElectionValidation: LeaderElectionValidationAlgebra[F],
    ed25519VRFResource:       UnsafeResource[F, Ed25519VRF],
    vrfConfig:                VrfConfig,
    thresholdInterpreter:     LeaderElectionValidationAlgebra[F],
    vrfProofsCache:           CaffeineCache[F, Bytes, Map[Long, Proofs.Knowledge.VrfEd25519]],
    rhosCache:                CaffeineCache[F, Bytes, Map[Long, Rho]]
  ) extends VrfCalculatorAlgebra[F] {

    def proofForSlot(slot: Slot, eta: Eta): F[Proofs.Knowledge.VrfEd25519] =
      OptionT(vrfProofsCache.get(eta.data))
        .orElseF {
          for {
            epoch    <- clock.epochOf(slot)
            _        <- Logger[F].info(show"Computing VRF Proofs values for epoch=$epoch eta=$eta")
            boundary <- clock.epochRange(epoch)
            vrfProofs <- ed25519VRFResource.use { implicit ed =>
              boundary
                .map { slot =>
                  slot -> compute(
                    VrfArgument(eta, slot),
                    ed
                  )
                }
                .toMap
                .pure[F]
            }
            _ <- vrfProofsCache.put(eta.data)(vrfProofs)
          } yield vrfProofs.some // TODO: Ask why the previous implementation Get from cache again if the values was already there.
        }
        .subflatMap(_.get(slot))
        .getOrElseF(
          new IllegalStateException(show"proof was not precomputed for slot=$slot eta=$eta")
            .raiseError[F, Proofs.Knowledge.VrfEd25519]
        )

    def rhoForSlot(slot: Slot, eta: Eta): F[Rho] =
      OptionT(rhosCache.get(eta.data))
        .orElseF(
          clock
            .epochOf(slot)
            .flatMap(getAndPutRhoForSlotEta(_, eta))
        )
        .subflatMap(_.get(slot))
        .getOrElseF(
          new IllegalStateException(show"rho was not precomputed for slot=$slot eta=$eta")
            .raiseError[F, Rho]
        )

    private def getAndPutRhoForSlotEta(epoch: Epoch, eta: Eta): F[Option[Map[Slot, Rho]]] =
      for {
        _        <- Logger[F].info(show"Precomputing Rho values for epoch=$epoch eta=$eta")
        boundary <- clock.epochRange(epoch)
        rhoValues <- ed25519VRFResource.use { implicit ed =>
          boundary
            .map { slot =>
              slot -> compute(
                VrfArgument(eta, slot),
                ed
              )
            }
            .toMap
            .map { case (slot, proof) =>
              slot -> Rho(Sized.strictUnsafe(ed.proofToHash(proof.bytes.data)))
            }
            .pure[F]
        }
        _ <- rhosCache.put(eta.data)(rhoValues) // TODO: Ask rhoForSlot is responsible of save proofs in cache?
      } yield rhoValues.some // TODO: Ask why the previous implementation Get from cache again if the values was already there.

    private def compute(
      arg:        VrfArgument,
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
            .orElseF(
              getAndPutRhoForSlotEta(epoch, eta)
            ) // TODO: Ask why the previous implementation Get from cache again if the values was already there.
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

    def getHit(relativeStake: Ratio, slot: Slot, slotDiff: Long, eta: Eta): F[Option[VrfHit]] =
      (
        thresholdInterpreter.getThreshold(relativeStake, slotDiff),
        proofForSlot(slot, eta),
        rhoForSlot(slot, eta)
      ).tupled
        .flatMap { case (threshold, testProof, rho) =>
          thresholdInterpreter
            .isSlotLeaderForThreshold(threshold)(rho)
            .map(isLeader =>
              Option.when(isLeader)(
                VrfHit(
                  EligibilityCertificate(testProof, vkVrf, threshold.typedEvidence.evidence, eta),
                  slot,
                  threshold
                )
              )
            )
        }
  }
}

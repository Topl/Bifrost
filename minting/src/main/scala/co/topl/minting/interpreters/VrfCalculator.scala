package co.topl.minting.interpreters

import cats.Parallel
import cats.effect.{Resource, Sync}
import cats.implicits._
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.algebras.{ClockAlgebra, UnsafeResource}
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.consensus.algebras.LeaderElectionValidationAlgebra
import co.topl.consensus.models.{SignatureVrfEd25519, VrfArgument, VrfConfig}
import VrfArgument._
import cats.effect.implicits.effectResourceOps
import co.topl.crypto.signing.Ed25519VRF
import co.topl.minting.algebras.VrfCalculatorAlgebra
import co.topl.models._
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.{Ratio, Sized}
import co.topl.models.utility._
import com.github.benmanes.caffeine.cache.Caffeine
import scalacache.caffeine.CaffeineCache
import scala.collection.immutable.NumericRange
import scalacache.Entry

object VrfCalculator {

  private def caffeineCacheBuilder(vrfCacheSize: Long) = Caffeine.newBuilder.maximumSize(vrfCacheSize)

  def make[F[_]: Sync: Parallel](
    skVrf:                    SecretKeys.VrfEd25519,
    clock:                    ClockAlgebra[F],
    leaderElectionValidation: LeaderElectionValidationAlgebra[F],
    ed25519VRFResource:       UnsafeResource[F, Ed25519VRF],
    vrfConfig:                VrfConfig,
    vrfCacheSize:             Long
  ): Resource[F, VrfCalculatorAlgebra[F]] =
    for {
      vrfProofsCache <- Sync[F]
        .delay(
          CaffeineCache(caffeineCacheBuilder(vrfCacheSize).build[(Bytes, Long), Entry[SignatureVrfEd25519]]())
        )
        .toResource
      rhosCache <-
        Sync[F]
          .delay(
            CaffeineCache(caffeineCacheBuilder(vrfCacheSize).build[(Bytes, Long), Entry[Rho]]())
          )
          .toResource
      impl <- Resource.pure(
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
    } yield impl

  private class Impl[F[_]: Sync: Parallel](
    skVrf:                    SecretKeys.VrfEd25519,
    clock:                    ClockAlgebra[F],
    leaderElectionValidation: LeaderElectionValidationAlgebra[F],
    ed25519VRFResource:       UnsafeResource[F, Ed25519VRF],
    vrfConfig:                VrfConfig,
    vrfProofsCache:           CaffeineCache[F, (Bytes, Long), SignatureVrfEd25519],
    rhosCache:                CaffeineCache[F, (Bytes, Long), Rho]
  ) extends VrfCalculatorAlgebra[F] {

    def proofForSlot(slot: Slot, eta: Eta): F[SignatureVrfEd25519] =
      vrfProofsCache.cachingF((eta.data, slot))(ttl = None)(
        ed25519VRFResource.use(compute(VrfArgument(eta, slot), _))
      )

    def rhoForSlot(slot: Slot, eta: Eta): F[Rho] =
      rhosCache.cachingF((eta.data, slot))(ttl = None)(
        for {
          proof          <- proofForSlot(slot, eta)
          proofHashBytes <- ed25519VRFResource.use(_.proofToHash(proof.value).pure[F])
          rho = Rho(Sized.strictUnsafe(proofHashBytes))
        } yield rho
      )

    private def compute(
      arg:        VrfArgument,
      ed25519VRF: Ed25519VRF
    ): F[SignatureVrfEd25519] =
      Sync[F].delay(
        SignatureVrfEd25519.of(
          ed25519VRF
            .sign(
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
        boundary <- clock.epochRange(epoch)
        rhosList <- boundary.toList.traverse(slot => rhoForSlot(slot, eta).tupleLeft(slot))
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

package co.topl.consensus

import cats._
import cats.data._
import cats.effect.Ref
import cats.effect.kernel.Sync
import cats.implicits._
import co.topl.algebras.Store
import co.topl.consensus.algebras._
import co.topl.crypto.hash.blake2b256
import co.topl.crypto.signing.{Ed25519, Ed25519VRF, KesProduct}
import co.topl.models._
import co.topl.models.utility.Ratio
import co.topl.typeclasses.BlockGenesis
import co.topl.typeclasses.implicits._
import com.google.common.primitives.Longs
import scalacache.CacheConfig
import scalacache.caffeine.CaffeineCache

import scala.language.implicitConversions

/**
 * Interpreters for the ConsensusValidationAlgebra
 */
object BlockHeaderValidation {

  object Eval {

    // TODO: Validate incoming blocks are not past the *global* slot

    def make[F[_]: Monad: Sync](
      etaInterpreter:           EtaCalculationAlgebra[F],
      relativeStakeInterpreter: VrfRelativeStakeValidationLookupAlgebra[F],
      leaderElection:           LeaderElectionValidationAlgebra[F],
      registrationInterpreter:  RegistrationLookupAlgebra[F]
    ): F[BlockHeaderValidationAlgebra[F]] =
      (
        Ref.of[F, Ed25519VRF](Ed25519VRF.precomputed()),
        Ref.of[F, KesProduct](new KesProduct),
        Ref.of[F, Ed25519](new Ed25519)
      )
        .mapN((vrfRef, kesRef, ed25519Ref) =>
          new Impl[F](
            etaInterpreter,
            relativeStakeInterpreter,
            leaderElection,
            registrationInterpreter,
            vrfRef,
            kesRef,
            ed25519Ref
          )
        )

    private class Impl[F[_]: Monad: Sync](
      etaInterpreter:           EtaCalculationAlgebra[F],
      relativeStakeInterpreter: VrfRelativeStakeValidationLookupAlgebra[F],
      leaderElection:           LeaderElectionValidationAlgebra[F],
      registrationInterpreter:  RegistrationLookupAlgebra[F],
      vrfRef:                   Ref[F, Ed25519VRF],
      kesRef:                   Ref[F, KesProduct],
      ed25519Ref:               Ref[F, Ed25519]
    ) extends BlockHeaderValidationAlgebra[F] {

      def validate(
        child:  BlockHeaderV2,
        parent: BlockHeaderV2
      ): F[Either[BlockHeaderValidationFailure, BlockHeaderV2]] = {
        for {
          _         <- statelessVerification(child, parent)
          _         <- vrfVerification(child)
          _         <- kesVerification(child)
          _         <- registrationVerification(child)
          threshold <- EitherT.liftF(vrfThresholdFor(child, parent))
          _         <- vrfThresholdVerification(child, threshold)
          _         <- eligibilityVerification(child, threshold)
        } yield child
      }.value

      private[consensus] def statelessVerification(child: BlockHeaderV2, parent: BlockHeaderV2) =
        EitherT
          .pure[F, BlockHeaderValidationFailure](child)
          .ensure(BlockHeaderValidationFailures.NonForwardSlot(child.slot, parent.slot))(child =>
            child.slot > parent.slot
          )
          .ensureOr(child => BlockHeaderValidationFailures.NonForwardTimestamp(child.timestamp, parent.timestamp))(
            child => child.timestamp > parent.timestamp
          )
          .ensureOr(child => BlockHeaderValidationFailures.ParentMismatch(child.parentHeaderId, parent.id))(
            _.parentHeaderId === parent.id
          )
          .ensureOr(child => BlockHeaderValidationFailures.NonForwardHeight(child.height, parent.height))(
            _.height === parent.height + 1
          )

      /**
       * Verifies the given block's VRF certificate syntactic integrity for a particular stateful nonce
       */
      private[consensus] def vrfVerification(
        header: BlockHeaderV2
      ): EitherT[F, BlockHeaderValidationFailure, BlockHeaderV2] =
        EitherT
          .liftF(etaInterpreter.etaToBe(header.parentSlotId, header.slot))
          .flatMapF(expectedEta =>
            vrfRef.modify { implicit ed25519vrf =>
              val certificate = header.eligibilityCertificate
              ed25519vrf -> header
                .asRight[BlockHeaderValidationFailure]
                .ensure(
                  BlockHeaderValidationFailures
                    .InvalidEligibilityCertificateEta(header.eligibilityCertificate.eta, expectedEta)
                )(header => header.eligibilityCertificate.eta === expectedEta)
                .ensure(
                  BlockHeaderValidationFailures.InvalidEligibilityCertificateProof(certificate.vrfSig)
                )(header =>
                  ed25519vrf.verify(
                    certificate.vrfSig,
                    LeaderElectionValidation.VrfArgument(expectedEta, header.slot).signableBytes,
                    certificate.vkVRF
                  )
                )
            }
          )

      /**
       * Verifies the given block's Operational Certificate's parent -> linear commitment, and the Operational
       * Certificate's block signature
       */
      private[consensus] def kesVerification(
        header: BlockHeaderV2
      ): EitherT[F, BlockHeaderValidationFailure, BlockHeaderV2] =
        EitherT(
          kesRef
            .modify(kesProduct =>
              kesProduct -> kesProduct.verify(
                header.operationalCertificate.parentSignature,
                header.operationalCertificate.childVK.bytes.data ++ Bytes(Longs.toByteArray(header.slot)),
                header.operationalCertificate.parentVK
              )
            )
            .map(isValid =>
              if (isValid) header.asRight[BlockHeaderValidationFailure]
              else
                (BlockHeaderValidationFailures.InvalidOperationalParentSignature(
                  header.operationalCertificate
                ): BlockHeaderValidationFailure).asLeft[BlockHeaderV2]
            )
        )
          .flatMap(_ =>
            EitherT(
              ed25519Ref
                .modify(ed25519 =>
                  ed25519 -> ed25519.verify(
                    header.operationalCertificate.childSignature,
                    header.signableBytes,
                    header.operationalCertificate.childVK
                  )
                )
                .map(isValid =>
                  if (isValid) header.asRight[BlockHeaderValidationFailure]
                  else
                    (BlockHeaderValidationFailures.InvalidBlockProof(
                      header.operationalCertificate
                    ): BlockHeaderValidationFailure).asLeft[BlockHeaderV2]
                )
            )
          )

      /**
       * Determines the VRF threshold for the given child
       */
      private def vrfThresholdFor(child: BlockHeaderV2, parent: BlockHeaderV2): F[Ratio] =
        relativeStakeInterpreter
          .lookupAt(SlotId(child.slot, child.id), child.address)
          .flatMap(relativeStake =>
            leaderElection.getThreshold(
              relativeStake.getOrElse(Ratio(0)),
              child.slot - parent.slot
            )
          )

      /**
       * Verify that the threshold evidence stamped on the block matches the threshold generated using local state
       */
      private[consensus] def vrfThresholdVerification(
        header:    BlockHeaderV2,
        threshold: Ratio
      ): EitherT[F, BlockHeaderValidationFailure, BlockHeaderV2] =
        EitherT.cond(
          header.eligibilityCertificate.thresholdEvidence === threshold.typedEvidence.evidence,
          header,
          BlockHeaderValidationFailures.InvalidVrfThreshold(threshold)
        )

      /**
       * Verify that the block's staker is eligible using their relative stake distribution
       */
      private[consensus] def eligibilityVerification(
        header:    BlockHeaderV2,
        threshold: Ratio
      ): EitherT[F, BlockHeaderValidationFailure, BlockHeaderV2] =
        EitherT
          .liftF(
            vrfRef
              .modify { implicit ed25519Vrf =>
                ed25519Vrf -> ed25519Vrf.proofToHash(header.eligibilityCertificate.vrfSig)
              }
              .flatMap(leaderElection.isSlotLeaderForThreshold(threshold))
          )
          .ensure(
            BlockHeaderValidationFailures
              .IneligibleCertificate(threshold, header.eligibilityCertificate): BlockHeaderValidationFailure
          )(
            identity
          )
          .map(_ => header)

      /**
       * Verifies the staker's registration.  First checks that the staker is registered at all.  Once retrieved,
       * the registration contains a commitment/proof that must be verified using
       * the 0th timestep of the header's operational certificate's "parentVK".  The proof's message is the hash of
       * (the staker's vrfVK concatenated with the staker's poolVK).
       */
      private[consensus] def registrationVerification(
        header: BlockHeaderV2
      ): EitherT[F, BlockHeaderValidationFailure, BlockHeaderV2] =
        OptionT(
          registrationInterpreter.registrationOf(SlotId(header.slot, header.id), header.address)
        )
          .map(_.commitment)
          .toRight(BlockHeaderValidationFailures.Unregistered(header.address): BlockHeaderValidationFailure)
          .flatMapF { commitment =>
            val message = Bytes(
              blake2b256
                .hash(
                  (header.eligibilityCertificate.vkVRF.bytes.data ++ header.address.poolVK.bytes.data).toArray
                )
                .value
            )
            kesRef
              .modify(p => (p, p.verify(commitment, message, header.operationalCertificate.parentVK.copy(step = 0))))
              .map(isValid =>
                Either.cond(
                  isValid,
                  header,
                  BlockHeaderValidationFailures
                    .RegistrationCommitmentMismatch(
                      commitment,
                      header.eligibilityCertificate.vkVRF,
                      header.address.poolVK
                    )
                )
              )
          }
    }
  }

  object WithCache {

    implicit private val cacheConfig: CacheConfig = CacheConfig(cacheKeyBuilder[TypedIdentifier])

    def make[F[_]: MonadError[*[_], Throwable]: Sync](
      underlying:       BlockHeaderValidationAlgebra[F],
      blockHeaderStore: Store[F, BlockHeaderV2]
    ): F[BlockHeaderValidationAlgebra[F]] =
      CaffeineCache[F, TypedIdentifier].map(implicit cache =>
        new BlockHeaderValidationAlgebra[F] {

          def validate(
            child:  BlockHeaderV2,
            parent: BlockHeaderV2
          ): F[Either[BlockHeaderValidationFailure, BlockHeaderV2]] =
            OptionT(scalacache.get[F, TypedIdentifier](child.id))
              .map(_ => child.asRight[BlockHeaderValidationFailure])
              .getOrElseF(
                validateParent(parent)
                  .flatMapF(_ => underlying.validate(child, parent))
                  .semiflatTap(h => scalacache.put(h.id)(h.id))
                  .value
              )

          private def validateParent(parent: BlockHeaderV2): EitherT[F, BlockHeaderValidationFailure, BlockHeaderV2] =
            if (parent.parentHeaderId === BlockGenesis.ParentId)
              EitherT.pure[F, BlockHeaderValidationFailure](parent)
            else
              EitherT(
                OptionT(blockHeaderStore.get(parent.parentHeaderId))
                  .getOrElseF(
                    new IllegalStateException(s"Non-existent block header id=${parent.parentHeaderId}")
                      .raiseError[F, BlockHeaderV2]
                  )
                  .flatMap(grandParent => Sync[F].defer(validate(parent, grandParent)))
              )
        }
      )
  }
}

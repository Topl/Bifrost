package co.topl.consensus

import cats._
import cats.data.{Store => CStore, _}
import cats.effect.Ref
import cats.effect.kernel.Sync
import cats.implicits._
import co.topl.algebras.Store
import co.topl.consensus.algebras._
import co.topl.crypto.hash.blake2b256
import co.topl.crypto.signing.Ed25519VRF
import co.topl.models._
import co.topl.models.utility.Ratio
import co.topl.typeclasses.BlockGenesis
import co.topl.typeclasses.implicits._
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
    ): F[BlockHeaderValidationAlgebra[F]] = Ref
      .of[F, Ed25519VRF](Ed25519VRF.precomputed())
      .map(ref =>
        new BlockHeaderValidationAlgebra[F] {

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
                ref.modify { implicit ed25519vrf =>
                  val certificate = header.eligibilityCertificate
                  ed25519vrf -> header
                    .asRight[BlockHeaderValidationFailure]
                    .ensure(
                      BlockHeaderValidationFailures
                        .InvalidEligibilityCertificateEta(header.eligibilityCertificate.eta, expectedEta)
                    )(header => header.eligibilityCertificate.eta === expectedEta)
                    .ensure(
                      BlockHeaderValidationFailures.InvalidEligibilityCertificateTestProof(certificate.vrfTestSig)
                    )(header =>
                      ed25519vrf.verify(
                        certificate.vrfTestSig,
                        LeaderElectionValidation
                          .VrfArgument(expectedEta, header.slot, LeaderElectionValidation.Tokens.Test)
                          .signableBytes,
                        certificate.vkVRF
                      )
                    )
                    .ensure(
                      BlockHeaderValidationFailures.InvalidEligibilityCertificateNonceProof(certificate.vrfNonceSig)
                    )(header =>
                      ed25519vrf.verify(
                        certificate.vrfNonceSig,
                        LeaderElectionValidation
                          .VrfArgument(expectedEta, header.slot, LeaderElectionValidation.Tokens.Nonce)
                          .signableBytes,
                        certificate.vkVRF
                      )
                    )
                }
              )

          /**
           * Verifies the given block's KES certificate syntactic integrity for a particular stateful nonce
           */
          private[consensus] def kesVerification(
            header: BlockHeaderV2
          ): EitherT[F, BlockHeaderValidationFailure, BlockHeaderV2] =
            // Did the skHD
            // Does the KES proof satisfy vkHD using data (vkKES)
            EitherT
              .cond[F](
                true,
                header,
                BlockHeaderValidationFailures.InvalidKesCertificateKESProof(
                  header.operationalCertificate
                ): BlockHeaderValidationFailure
              )
              // TODO: Is `vki` committed to?
              // MMM Verification
              // Check signature against block bytes
              .ensureOr(header =>
                BlockHeaderValidationFailures.InvalidKesCertificateMMMProof(header.operationalCertificate)
              )(header => true)

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
                ref
                  .modify { implicit ed25519Vrf =>
                    ed25519Vrf -> ed25519Vrf.proofToHash(header.eligibilityCertificate.vrfTestSig)
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
           * Verifies the staker's registration
           * 1. Does the hash of the vkvrf that was included in the block header == TaktikosRegistration.vrfCommitment in the registration box
           * 2. Is the vki (header.cert.vkHD) in the set committed to by vkm.  What is the index for extended VK?
           *      TaktikosRegistration.extendedVk.evolve(index) == header.cert.vkHD
           */
          private[consensus] def registrationVerification(
            header: BlockHeaderV2
          ): EitherT[F, BlockHeaderValidationFailure, BlockHeaderV2] =
            OptionT(
              registrationInterpreter.registrationOf(SlotId(header.slot, header.id), header.address)
            )
              .map(_.commitment)
              .toRight(BlockHeaderValidationFailures.Unregistered(header.address): BlockHeaderValidationFailure)
              .ensureOr(
                BlockHeaderValidationFailures
                  .RegistrationCommitmentMismatch(_, header.eligibilityCertificate.vkVRF, header.address.poolVK)
              )(_ => true
              // TODO
//                _.data.toArray === blake2b256
//                  .hash((header.eligibilityCertificate.vkVRF.bytes.data ++ header.address.poolVK.bytes.data).toArray)
//                  .value
              )
              .map(_ => header)

        }
      )
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

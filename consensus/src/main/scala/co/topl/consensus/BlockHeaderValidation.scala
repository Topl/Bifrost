package co.topl.consensus

import cats._
import cats.data._
import cats.implicits._
import co.topl.consensus.algebras._
import co.topl.consensus.vrf.ProofToHash
import co.topl.crypto.hash.blake2b256
import co.topl.models._
import co.topl.models.utility.Ratio
import co.topl.typeclasses.implicits._

import scala.language.implicitConversions

/**
 * Interpreters for the ConsensusValidationAlgebra
 */
object BlockHeaderValidation {

  object Eval {

    object Stateless {

      def make[F[_]: Monad]: BlockHeaderValidationAlgebra[F] =
        (child: BlockHeaderV2, parent: BlockHeaderV2) =>
          EitherT
            .pure[F, BlockHeaderValidationFailure](child)
            .ensure(BlockHeaderValidationFailures.NonForwardSlot(child.slot, parent.slot))(child =>
              child.slot > parent.slot
            )
            .ensureOr(child => BlockHeaderValidationFailures.NonForwardTimestamp(child.timestamp, parent.timestamp))(
              child => child.timestamp > parent.timestamp
            )
            .ensureOr(child => BlockHeaderValidationFailures.ParentMismatch(child.parentHeaderId, parent.id))(
              _.parentHeaderId == parent.id
            )
            .ensureOr(child => BlockHeaderValidationFailures.NonForwardHeight(child.height, parent.height))(
              _.height == parent.height + 1
            )
            .value
    }

    object MinimalState {

      // TODO: Validate incoming blocks are not past the *global* slot
      def make[F[_]: Monad](
        etaInterpreter: EtaValidationAlgebra[F]
      ): BlockHeaderValidationAlgebra[F] = new BlockHeaderValidationAlgebra[F] {
        private val statelessInterpreter = Stateless.make[F]

        def validate(
          child:  BlockHeaderV2,
          parent: BlockHeaderV2
        ): F[Either[BlockHeaderValidationFailure, BlockHeaderV2]] =
          EitherT(statelessInterpreter.validate(child, parent))
            .flatMap(vrfVerification)
            .flatMap(kesVerification)
            .value

        /**
         * Verifies the given block's VRF certificate syntactic integrity for a particular stateful nonce
         */
        private[consensus] def vrfVerification(
          header: BlockHeaderV2
        ): EitherT[F, BlockHeaderValidationFailure, BlockHeaderV2] =
          EitherT
            .liftF(etaInterpreter.etaOf((header.slot, header.id)))
            .subflatMap { eta =>
              val certificate = header.eligibilityCertificate
              header
                .asRight[BlockHeaderValidationFailure]
                .ensure(
                  BlockHeaderValidationFailures
                    .InvalidEligibilityCertificateEta(header.eligibilityCertificate.eta, eta)
                )(header => header.eligibilityCertificate.eta === eta)
                .ensure(BlockHeaderValidationFailures.InvalidEligibilityCertificateTestProof(certificate.vrfTestSig))(
                  header =>
                    certificate.vrfTestSig.satisfies(
                      certificate.vkVRF.proposition,
                      LeaderElectionValidation.VrfArgument(eta, header.slot, LeaderElectionValidation.Tokens.Test)
                    )
                )
                .ensure(BlockHeaderValidationFailures.InvalidEligibilityCertificateNonceProof(certificate.vrfNonceSig))(
                  header =>
                    certificate.vrfNonceSig.satisfies(
                      certificate.vkVRF.proposition,
                      LeaderElectionValidation.VrfArgument(eta, header.slot, LeaderElectionValidation.Tokens.Nonce)
                    )
                )
            }

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
      }
    }

    object Stateful {

      def make[F[_]: Monad](
        epochNoncesInterpreter:   EtaValidationAlgebra[F],
        relativeStakeInterpreter: VrfRelativeStakeValidationLookupAlgebra[F],
        leaderElection:           LeaderElectionValidationAlgebra[F],
        registrationInterpreter:  RegistrationLookupAlgebra[F]
      ): BlockHeaderValidationAlgebra[F] = new BlockHeaderValidationAlgebra[F] {

        private val minimalStateInterpreter = MinimalState.make[F](epochNoncesInterpreter)

        def validate(
          child:  BlockHeaderV2,
          parent: BlockHeaderV2
        ): F[Either[BlockHeaderValidationFailure, BlockHeaderV2]] =
          EitherT(
            minimalStateInterpreter
              .validate(child, parent)
          )
            .flatMap(child =>
              registrationVerification(child).flatMap(child =>
                EitherT
                  .liftF(vrfThresholdFor(child, parent))
                  .flatMap(threshold =>
                    vrfThresholdVerification(child, threshold)
                      .flatMap(header => eligibilityVerification(header, threshold))
                  )
              )
            )
            .value

        /**
         * Determines the VRF threshold for the given child
         */
        private def vrfThresholdFor(child: BlockHeaderV2, parent: BlockHeaderV2): F[Ratio] =
          relativeStakeInterpreter
            .lookupAt((child.slot, child.id), child.address)
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
            header.eligibilityCertificate.thresholdEvidence === threshold.evidence,
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
              leaderElection
                .isSlotLeaderForThreshold(threshold)(ProofToHash.digest(header.eligibilityCertificate.vrfTestSig))
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
            registrationInterpreter.registrationOf((header.slot, header.id), header.address)
          )
            .map(_.vrfCommitment)
            .toRight(BlockHeaderValidationFailures.Unregistered(header.address): BlockHeaderValidationFailure)
            .ensureOr(
              BlockHeaderValidationFailures.RegistrationCommitmentMismatch(_, header.eligibilityCertificate.vkVRF)
            )(
              _.data.toArray === blake2b256
                .hash(header.eligibilityCertificate.vkVRF.bytes.data.toArray)
                .value
            )
            .map(_ => header)

      }
    }
  }
}

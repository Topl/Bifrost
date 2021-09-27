package co.topl.consensus

import cats.MonadError
import cats.data.OptionT
import cats.implicits._
import co.topl.consensus.algebras._
import co.topl.consensus.vrf.ProofToHash
import co.topl.crypto.hash.blake2b256
import co.topl.crypto.typeclasses.implicits._
import co.topl.models._
import co.topl.models.utility.{Lengths, Ratio, Sized}
import co.topl.typeclasses.implicits._

import scala.language.implicitConversions

/**
 * Interpreters for the ConsensusValidationAlgebra
 */
object BlockHeaderValidation {

  object Eval {

    object Stateless {

      def make[F[_]: MonadError[*[_], Failure]]: BlockHeaderValidationAlgebra[F] =
        (child: BlockHeaderV2, parent: BlockHeaderV2) =>
          child
            .pure[F]
            .ensure(Failures.NonForwardSlot(child.slot, parent.slot))(child => child.slot > parent.slot)
            .ensureOr(child => Failures.NonForwardTimestamp(child.timestamp, parent.timestamp))(child =>
              child.timestamp > parent.timestamp
            )
            .ensureOr(child => Failures.ParentMismatch(child.parentHeaderId, parent.id))(_.parentHeaderId == parent.id)
            .ensureOr(child => Failures.NonForwardHeight(child.height, parent.height))(
              _.height == parent.height + 1
            )
    }

    object MinimalState {

      // TODO: Validate incoming blocks are not past the *global* slot
      def make[F[_]: MonadError[*[_], Failure]](
        etaInterpreter: EtaValidationAlgebra[F]
      ): BlockHeaderValidationAlgebra[F] = new BlockHeaderValidationAlgebra[F] {
        private val statelessInterpreter = Stateless.make[F]

        def validate(child: BlockHeaderV2, parent: BlockHeaderV2): F[BlockHeaderV2] =
          statelessInterpreter
            .validate(child, parent)
            .flatMap(vrfVerification)
            .flatMap(kesVerification)

        /**
         * Verifies the given block's VRF certificate syntactic integrity for a particular stateful nonce
         */
        private[consensus] def vrfVerification(header: BlockHeaderV2): F[BlockHeaderV2] =
          etaInterpreter
            .etaOf((header.slot, header.id))
            .flatMap { eta =>
              val certificate = header.eligibibilityCertificate
              header
                .pure[F]
                .ensure(Failures.InvalidEligibilityCertificateEta(header.eligibibilityCertificate.eta, eta))(header =>
                  header.eligibibilityCertificate.eta === eta
                )
                .ensure(Failures.InvalidEligibilityCertificateTestProof(certificate.vrfTestSig))(header =>
                  certificate.vrfTestSig.satisfies(
                    certificate.vkVRF.proposition,
                    LeaderElectionValidation.VrfArgument(eta, header.slot, LeaderElectionValidation.Tokens.Test)
                  )
                )
                .ensure(Failures.InvalidEligibilityCertificateNonceProof(certificate.vrfNonceSig))(header =>
                  certificate.vrfNonceSig.satisfies(
                    certificate.vkVRF.proposition,
                    LeaderElectionValidation.VrfArgument(eta, header.slot, LeaderElectionValidation.Tokens.Nonce)
                  )
                )
            }

        /**
         * Verifies the given block's KES certificate syntactic integrity for a particular stateful nonce
         */
        private[consensus] def kesVerification(header: BlockHeaderV2): F[BlockHeaderV2] =
          header
            .pure[F]
            //
            .ensureOr(header => Failures.InvalidKesCertificateKESProof(header.operationalCertificate))(header =>
              // Did the skHD
              // Does the KES proof satisfy vkHD using data (vkKES)
              true
            )
            // TODO: Is `vki` committed to?
            // MMM Verification
            // Check signature against block bytes
            .ensureOr(header => Failures.InvalidKesCertificateMMMProof(header.operationalCertificate))(header => true)
      }
    }

    object Stateful {

      def make[F[_]: MonadError[*[_], Failure]](
        epochNoncesInterpreter:   EtaValidationAlgebra[F],
        relativeStakeInterpreter: VrfRelativeStakeValidationLookupAlgebra[F],
        leaderElection:           LeaderElectionValidationAlgebra[F],
        registrationInterpreter:  RegistrationLookupAlgebra[F]
      ): BlockHeaderValidationAlgebra[F] = new BlockHeaderValidationAlgebra[F] {

        private val minimalStateInterpreter = MinimalState.make[F](epochNoncesInterpreter)

        def validate(child: BlockHeaderV2, parent: BlockHeaderV2): F[BlockHeaderV2] =
          minimalStateInterpreter
            .validate(child, parent)
            .flatMap(child =>
              registrationVerification(child).flatMap(child =>
                vrfThresholdFor(child, parent)
                  .flatMap(threshold =>
                    vrfThresholdVerification(child, threshold)
                      .flatMap(header => eligibilityVerification(header, threshold))
                  )
              )
            )

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
        private[consensus] def vrfThresholdVerification(header: BlockHeaderV2, threshold: Ratio): F[BlockHeaderV2] =
          header
            .pure[F]
            .ensure(Failures.InvalidVrfThreshold(threshold))(header =>
              header.eligibibilityCertificate.thresholdEvidence === threshold.evidence
            )

        /**
         * Verify that the block's staker is eligible using their relative stake distribution
         */
        private[consensus] def eligibilityVerification(header: BlockHeaderV2, threshold: Ratio): F[BlockHeaderV2] =
          leaderElection
            .isSlotLeaderForThreshold(threshold)(ProofToHash.digest(header.eligibibilityCertificate.vrfTestSig))
            .ensure(Failures.IneligibleCertificate(threshold, header.eligibibilityCertificate))(identity)
            .map(_ => header)

        /**
         * Verifies the staker's registration
         * 1. Does the hash of the vkvrf that was included in the block header == TaktikosRegistration.vrfCommitment in the registration box
         * 2. Is the vki (header.cert.vkHD) in the set committed to by vkm.  What is the index for extended VK?
         *      TaktikosRegistration.extendedVk.evolve(index) == header.cert.vkHD
         */
        private[consensus] def registrationVerification(header: BlockHeaderV2): F[BlockHeaderV2] =
          OptionT(registrationInterpreter.registrationOf((header.slot, header.id), header.address))
            .map(_.vrfCommitment)
            .ensureOr(
              Failures.RegistrationCommitmentMismatch(_, header.eligibibilityCertificate.vkVRF)
            )(
              _.data.toArray === blake2b256
                .hash(header.eligibibilityCertificate.vkVRF.ed25519.bytes.data.toArray)
                .value
            )
            .getOrElseF(Failures.Unregistered(header.address).raiseError[F, BlockHeaderV2])

      }
    }
  }

  sealed abstract class Failure

  object Failures {
    case class NonForwardSlot(slot: Slot, parentSlot: Slot) extends Failure

    case class NonForwardTimestamp(timestamp: Timestamp, parentTimestamp: Timestamp) extends Failure

    case class NonForwardHeight(height: Long, parentHeight: Long) extends Failure

    case class ParentMismatch(expectedParentId: TypedIdentifier, parentId: TypedIdentifier) extends Failure

    case class InvalidVrfThreshold(threshold: Ratio) extends Failure

    case class IneligibleCertificate(threshold: Ratio, eligibilityCertificate: EligibilityCertificate) extends Failure

    case class InvalidEligibilityCertificateEta(claimedEta: Eta, actualEta: Eta) extends Failure

    case class InvalidEligibilityCertificateTestProof(proof: Proofs.Signature.VrfEd25519) extends Failure

    case class InvalidEligibilityCertificateNonceProof(proof: Proofs.Signature.VrfEd25519) extends Failure

    case class InvalidKesCertificateKESProof(kesCertificate: OperationalCertificate) extends Failure

    case class InvalidKesCertificateMMMProof(kesCertificate: OperationalCertificate) extends Failure

    case class Unregistered(address: TaktikosAddress) extends Failure

    case class RegistrationCommitmentMismatch(
      vrfCommitment: Sized.Strict[Bytes, Lengths.`32`.type],
      vkVrf:         VerificationKeys.Vrf
    ) extends Failure

  }
}

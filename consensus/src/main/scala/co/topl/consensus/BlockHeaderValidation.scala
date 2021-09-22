package co.topl.consensus

import cats.MonadError
import cats.implicits._
import co.topl.consensus.algebras._
import co.topl.consensus.vrf.ProofToHash
import co.topl.crypto.kes.KesVerifier
import co.topl.crypto.typeclasses.implicits._
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
              val certificate = header.vrfCertificate
              header
                .pure[F]
                .ensureOr(header => Failures.InvalidVrfCertificateTestProof(header.vrfCertificate.testProof))(header =>
                  certificate.testProof.satisfies(
                    certificate.vkVRF.proposition,
                    LeaderElectionValidation.VrfArgument(eta, header.slot, LeaderElectionValidation.Tokens.Test)
                  )
                )
                .ensureOr(header => Failures.InvalidVrfCertificateNonceProof(header.vrfCertificate.nonceProof))(
                  header =>
                    certificate.nonceProof.satisfies(
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
            .ensureOr(header => Failures.InvalidKesCertificateKESProof(header.kesCertificate))(header =>
              // Did the skHD
              // Does the KES proof satisfy vkHD using data (vkKES)
              true ||
              header.kesCertificate.kesProof
                .satisfies(Propositions.PublicKeyEd25519(header.kesCertificate.vkHD), header.kesCertificate.vkKES)
            )
            // TODO: Is `vki` committed to?
            // MMM Verification
            // Check signature against block bytes
            .ensureOr(header => Failures.InvalidKesCertificateMMMProof(header.kesCertificate))(header =>
              KesVerifier.verify(header, header.kesCertificate.mmmProof, header.slot)
            )
      }
    }

    object Stateful {

      def make[F[_]: MonadError[*[_], Failure]](
        epochNoncesInterpreter:   EtaValidationAlgebra[F],
        relativeStakeInterpreter: VrfRelativeStakeValidationLookupAlgebra[F],
        leaderElection:           LeaderElectionValidationAlgebra[F]
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
            .ensure(Failures.InvalidVrfThreshold(threshold))(header => header.thresholdEvidence == threshold.evidence)

        /**
         * Verify that the block's staker is eligible using their relative stake distribution
         */
        private[consensus] def eligibilityVerification(header: BlockHeaderV2, threshold: Ratio): F[BlockHeaderV2] =
          leaderElection
            .isSlotLeaderForThreshold(threshold)(ProofToHash.digest(header.vrfCertificate.testProof))
            .ensure(Failures.IneligibleVrfCertificate(threshold, header.vrfCertificate))(identity)
            .map(_ => header)

        /**
         * Verifies the staker's registration
         * 1. Does the hash of the vkvrf that was included in the block header == TaktikosRegistration.vrfCommitment in the registration box
         * 2. Is the vki (header.cert.vkHD) in the set committed to by vkm.  What is the index for extended VK?
         *      TaktikosRegistration.extendedVk.evolve(index) == header.cert.vkHD
         */
        private[consensus] def registrationVerification(header: BlockHeaderV2): F[BlockHeaderV2] =
          header.pure[F]

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

    case class IneligibleVrfCertificate(threshold: Ratio, vrfCertificate: Vrf.Certificate) extends Failure

    case class InvalidVrfCertificateTestProof(proof: Proofs.Vrf.Test) extends Failure

    case class InvalidVrfCertificateNonceProof(proof: Proofs.Vrf.Nonce) extends Failure

    case class InvalidKesCertificateKESProof(kesCertificate: KesCertificate) extends Failure

    case class InvalidKesCertificateMMMProof(kesCertificate: KesCertificate) extends Failure

    case class IncompleteEpochData(epoch: Epoch) extends Failure
  }
}

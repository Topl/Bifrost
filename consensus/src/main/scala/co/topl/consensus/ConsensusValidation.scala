package co.topl.consensus

import cats.MonadError
import cats.implicits._
import co.topl.consensus.vrf.ProofToHash
import co.topl.crypto.kes.KesVerifier
import co.topl.crypto.typeclasses.ProofVerifier.Instances._
import co.topl.crypto.typeclasses.ProofVerifier.ops._
import co.topl.crypto.typeclasses.Signable.instances._
import co.topl.models._
import co.topl.models.utility.Ratio
import co.topl.typeclasses.ContainsEvidence.Instances._
import co.topl.typeclasses.ContainsEvidence.ops._
import co.topl.typeclasses.Identifiable.Instances._
import co.topl.typeclasses.Identifiable.ops._

import java.nio.charset.StandardCharsets
import scala.language.implicitConversions

trait ConsensusValidation[F[_]] {

  /**
   * Indicates if the claimed child is a valid descendent of the parent
   */
  def validate(child: BlockHeaderV2, parent: BlockHeaderV2): F[ValidatedBlockHeader]
}

/**
 * A program which validates if a child block header can be chained to a parent block header
 */
object ConsensusValidation {

  object Eval {

    sealed abstract class Failure

    object Failures {
      case class NonForwardSlot(slot: Slot, parentSlot: Slot) extends Failure

      case class NonForwardTimestamp(timestamp: Timestamp, parentTimestamp: Timestamp) extends Failure

      case class ParentMismatch(expectedParentId: TypedIdentifier, parentId: TypedIdentifier) extends Failure

      case class InvalidVrfThreshold(threshold: Ratio) extends Failure

      case class IneligibleVrfCertificate(threshold: Ratio, vrfCertificate: Vrf.Certificate) extends Failure

      case class InvalidVrfCertificate(vrfCertificate: Vrf.Certificate) extends Failure

      case class InvalidKesCertificateKESProof(kesCertificate: KesCertificate) extends Failure

      case class InvalidKesCertificateMMMProof(kesCertificate: KesCertificate) extends Failure

      case class IncompleteEpochData(epoch: Epoch) extends Failure
    }

    def make[F[_]: MonadError[*[_], Failure]](
      epochNoncesInterpreter:   EtaAlgebra[F],
      relativeStakeInterpreter: VrfRelativeStakeLookupAlgebra[F],
      leaderElection:           LeaderElection[F]
    ): ConsensusValidation[F] = new ConsensusValidation[F] {

      def validate(child: BlockHeaderV2, parent: BlockHeaderV2): F[ValidatedBlockHeader] =
        statelessValidate(child, parent)
          .map(_.header)
          .flatMap(minimalStateValidate(_).map(_.header))
          .flatMap(fullStateValidate(_, parent))

      /**
       * Validations which require no state other than a "parent" block (syntax-based)
       */
      private[consensus] def statelessValidate(
        child:  BlockHeaderV2,
        parent: BlockHeaderV2
      ): F[ValidatedBlockHeader] =
        child
          .pure[F]
          .ensure(Failures.NonForwardSlot(child.slot, parent.slot))(child => child.slot > parent.slot)
          .ensureOr(child => Failures.NonForwardTimestamp(child.timestamp, parent.timestamp))(child =>
            child.timestamp > parent.timestamp
          )
          .ensureOr(child => Failures.ParentMismatch(child.parentHeaderId, parent.id))(_.parentHeaderId == parent.id)
          .map(ValidatedBlockHeader(_))

      /**
       * Validations which require just the epoch nonce
       */
      private[consensus] def minimalStateValidate(child: BlockHeaderV2): F[ValidatedBlockHeader] =
        vrfVerification(child)
          .flatMap(kesVerification)
          .map(ValidatedBlockHeader(_))

      /**
       * Validations which require a full consensus state (stake distribution and registration)
       */
      private[consensus] def fullStateValidate(
        child:  BlockHeaderV2,
        parent: BlockHeaderV2
      ): F[ValidatedBlockHeader] =
        registrationVerification(child).flatMap(child =>
          vrfThresholdFor(child, parent)
            .flatMap(threshold =>
              vrfThresholdVerification(child, threshold)
                .flatMap(header => eligibilityVerification(header, threshold))
            )
            .map(ValidatedBlockHeader(_))
        )

      /**
       * Determines the VRF threshold for the given child
       */
      private def vrfThresholdFor(
        child:  BlockHeaderV2,
        parent: BlockHeaderV2
      ): F[Ratio] =
        relativeStakeInterpreter
          .lookupAt(child)(child.address)
          .flatMap(relativeStake =>
            leaderElection.getThreshold(
              relativeStake,
              child.slot - parent.slot
            )
          )

      /**
       * Verify that the threshold evidence stamped on the block matches the threshold generated using local state
       */
      private[consensus] def vrfThresholdVerification(
        header:    BlockHeaderV2,
        threshold: Ratio
      ): F[BlockHeaderV2] = header
        .pure[F]
        .ensure(Failures.InvalidVrfThreshold(threshold))(header => header.thresholdEvidence == threshold.evidence)

      /**
       * Verify that the block's staker is eligible using their relative stake distribution
       */
      private[consensus] def eligibilityVerification(
        header:    BlockHeaderV2,
        threshold: Ratio
      ): F[BlockHeaderV2] =
        leaderElection
          .isSlotLeaderForThreshold(threshold)(ProofToHash.digest(header.vrfCertificate.testProof))
          .ensure(Failures.IneligibleVrfCertificate(threshold, header.vrfCertificate))(identity)
          .map(_ => header)

      /**
       * Verifies the staker's registration
       */
      private[consensus] def registrationVerification(
        header: BlockHeaderV2
      ): F[BlockHeaderV2] = header.pure[F]

      /**
       * Verifies the given block's VRF certificate syntactic integrity for a particular stateful nonce
       */
      private[consensus] def vrfVerification(
        header: BlockHeaderV2
      ): F[BlockHeaderV2] =
        epochNoncesInterpreter
          .etaOf(header)
          .flatMap { eta =>
            import co.topl.crypto.typeclasses.Proposes.implicits._
            import co.topl.crypto.typeclasses.Proposes.instances._
            val certificate = header.vrfCertificate
            header
              .pure[F]
              .ensureOr(header => Failures.InvalidVrfCertificate(header.vrfCertificate))(header =>
                certificate.testProof.satisfies(
                  certificate.vkVRF.proposition,
                  eta.data.toArray ++ BigInt(header.slot).toByteArray ++ "TEST".getBytes(StandardCharsets.UTF_8)
                ) && certificate.nonceProof.satisfies(
                  certificate.vkVRF.proposition,
                  eta.data.toArray ++ BigInt(header.slot).toByteArray ++ "NONCE".getBytes(StandardCharsets.UTF_8)
                )
              )
          }

      /**
       * Verifies the given block's KES certificate syntactic integrity for a particular stateful nonce
       */
      private[consensus] def kesVerification(
        header: BlockHeaderV2
      ): F[BlockHeaderV2] =
        header
          .pure[F]
          .ensureOr(header => Failures.InvalidKesCertificateKESProof(header.kesCertificate))(header =>
            true ||
            header.kesCertificate.kesProof
              .satisfies(Propositions.Consensus.PublicKeyKes(header.kesCertificate.vkKES), header)
          )
          .ensureOr(header => Failures.InvalidKesCertificateMMMProof(header.kesCertificate))(header =>
            KesVerifier.verify(header, header.kesCertificate.mmmProof, header.slot)
          )

    }
  }
}

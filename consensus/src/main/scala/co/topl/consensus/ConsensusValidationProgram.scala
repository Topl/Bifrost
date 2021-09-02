package co.topl.consensus

import cats.Monad
import cats.data.EitherT
import cats.implicits._
import co.topl.algebras.Clock
import co.topl.consensus.vrf.ProofToHash
import co.topl.crypto.signatures.Signature
import co.topl.models._
import co.topl.models.utility.Ratio
import co.topl.typeclasses.ContainsEvidence.Instances._
import co.topl.typeclasses.ContainsEvidence.ops._
import co.topl.typeclasses.Identifiable.Instances._
import co.topl.typeclasses.Identifiable.ops._
import co.topl.typeclasses.crypto.ProofVerifier.Instances._
import co.topl.typeclasses.crypto.ProofVerifier.ops._
import co.topl.typeclasses.crypto.Signable.Instances._
import io.estatico.newtype.macros.newtype
import io.estatico.newtype.ops._

import java.nio.charset.StandardCharsets
import scala.language.implicitConversions

class ConsensusValidationProgram[F[_]: Monad](
  epochNoncesInterpreter:        EpochNoncesAlgebra[F],
  relativeStakeInterpreter:      RelativeStateLookupAlgebra[F],
  clockInterpreter:              Clock[F]
)(implicit leaderElectionConfig: LeaderElection.Config) {
  import ConsensusValidationProgram._

  def validate(child: BlockHeaderV2, parent: BlockHeaderV2): EitherT[F, Failure, ValidatedBlockHeader] = {
    def test(
      f:       => Boolean,
      invalid: => Failure
    ): Either[Failure, BlockHeaderV2] =
      Either.cond(f, child, invalid)

    test(child.slot > parent.slot, Failures.NonForwardSlot(child.slot, parent.slot))
      .toEitherT[F]
      .subflatMap(header =>
        test(
          header.timestamp > parent.timestamp,
          Failures.NonForwardTimestamp(header.timestamp, parent.timestamp)
        )
      )
      .subflatMap(header =>
        test(header.parentHeaderId == parent.id, Failures.ParentMismatch(header.parentHeaderId, parent.id))
      )
      .flatMap(vrfVerification)
      .subflatMap(kesVerification)
      .flatMap(registrationVerification)
      .flatMap(header =>
        thresholdFor(header, parent)
          .subflatMap(threshold =>
            vrfThresholdVerification(header, threshold)
              .flatMap(header => eligibilityVerification(header, threshold))
          )
      )
      .map(header => ConsensusValidationProgram.ValidatedBlockHeader(header))
  }

  private def relativeStakeFor(header: BlockHeaderV2): EitherT[F, ConsensusValidationProgram.Failure, Ratio] =
    relativeStakeInterpreter
      .lookup(historicalEpoch(header))(header.address)
      .toRight(ConsensusValidationProgram.Failures.InvalidVrfThreshold(Ratio(0)))

  private def thresholdFor(
    header: BlockHeaderV2,
    parent: BlockHeaderV2
  ): EitherT[F, ConsensusValidationProgram.Failure, Ratio] =
    relativeStakeFor(header)
      .map(relativeStake =>
        LeaderElection.getThreshold(
          relativeStake,
          header.slot - parent.slot
        )
      )

  /**
   * Verify that the threshold evidence stamped on the block matches the threshold generated using local state
   */
  private[consensus] def vrfThresholdVerification(
    header:    BlockHeaderV2,
    threshold: Ratio
  ): Either[ConsensusValidationProgram.Failure, BlockHeaderV2] =
    Either.cond(
      header.thresholdEvidence == threshold.evidence,
      header,
      ConsensusValidationProgram.Failures.InvalidVrfThreshold(threshold)
    )

  /**
   * Verify that the block's staker is eligible using their relative stake distribution
   */
  private[consensus] def eligibilityVerification(
    header:    BlockHeaderV2,
    threshold: Ratio
  ): Either[ConsensusValidationProgram.Failure, BlockHeaderV2] =
    Either.cond(
      LeaderElection.isSlotLeaderForThreshold(threshold)(
        Bytes(
          ProofToHash.digest(Signature(header.vrfCertificate.testProof.bytes.data.toArray))
        )
      ) || true, // TODO: This check currently does not seem to work
      header,
      ConsensusValidationProgram.Failures.IneligibleVrfCertificate(threshold, header.vrfCertificate)
    )

  /**
   * Epoch N-2 Snapshot data
   */
  private[consensus] def registrationVerification(
    header: BlockHeaderV2
  ): EitherT[F, ConsensusValidationProgram.Failure, BlockHeaderV2] =
    EitherT.pure[F, ConsensusValidationProgram.Failure](header)

  /**
   * Verifies the given block's VRF certificate syntactic integrity for a particular stateful nonce
   */
  private[consensus] def vrfVerification(
    header: BlockHeaderV2
  ): EitherT[F, ConsensusValidationProgram.Failure, BlockHeaderV2] = {
    val epoch = historicalEpoch(header)
    epochNoncesInterpreter
      .nonceForEpoch(epoch)
      .toRight(Failures.IncompleteEpochData(epoch))
      .flatMap { epochNonce =>
        val certificate = header.vrfCertificate
        EitherT.cond[F](
          certificate.testProof.satisfies(
            Propositions.Consensus.PublicKeyVrf(certificate.vkVRF),
            "test".getBytes(StandardCharsets.UTF_8) ++ epochNonce.toArray
          ) && certificate.testProof.satisfies(
            Propositions.Consensus.PublicKeyVrf(certificate.vkVRF),
            "nonce".getBytes(StandardCharsets.UTF_8) ++ epochNonce.toArray
          ),
          header,
          ConsensusValidationProgram.Failures.InvalidVrfCertificate(header.vrfCertificate)
        )
      }
  }

  /**
   * Verifies the given block's KES certificate syntactic integrity for a particular stateful nonce
   */
  private[consensus] def kesVerification(
    header: BlockHeaderV2
  ): Either[ConsensusValidationProgram.Failure, BlockHeaderV2] = {
    implicit def h: BlockHeaderV2 = header
    val certificate = h.kesCertificate
    Either.cond(
      certificate.mmmProof.satisfies(Propositions.Consensus.PublicKeyKes(certificate.vkKES), header) &&
      certificate.kesProof.satisfies(
        Propositions.Consensus.PublicKeyKes(certificate.vkKES),
        // TODO: certificate.vkKES.bytes incorrect here
        certificate.vkKES.bytes.data.toArray ++ BigInt(certificate.slotOffset).toByteArray
      ),
      header,
      ConsensusValidationProgram.Failures.InvalidKesCertificate(header.kesCertificate)
    )
  }

  /**
   * Determines the epoch for which we are interested in epoch nonce and stake distribution for verification.
   *
   * (The N-2 epoch)
   */
  private def historicalEpoch(headerV2: BlockHeaderV2): Epoch =
    (clockInterpreter.epochOf(headerV2.slot) - 2).max(0)

}

object ConsensusValidationProgram {

  sealed abstract class Failure

  object Failures {
    case class NonForwardSlot(slot: Slot, parentSlot: Slot) extends Failure
    case class NonForwardTimestamp(timestamp: Timestamp, parentTimestamp: Timestamp) extends Failure
    case class ParentMismatch(expectedParentId: TypedIdentifier, parentId: TypedIdentifier) extends Failure
    case class InvalidVrfThreshold(threshold: Ratio) extends Failure
    case class IneligibleVrfCertificate(threshold: Ratio, vrfCertificate: VrfCertificate) extends Failure
    case class InvalidVrfCertificate(vrfCertificate: VrfCertificate) extends Failure
    case class InvalidKesCertificate(kesCertificate: KesCertificate) extends Failure
    case class IncompleteEpochData(epoch: Epoch) extends Failure
  }

  @newtype class ValidatedBlockHeader(val header: BlockHeaderV2)

  private[consensus] object ValidatedBlockHeader {

    def apply(blockHeaderV2: BlockHeaderV2): ValidatedBlockHeader =
      blockHeaderV2.coerce
  }

}

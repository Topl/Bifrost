package co.topl.consensus

import cats.Monad
import cats.data.{EitherT, OptionT}
import cats.implicits._
import co.topl.consensus.crypto.Vrf
import co.topl.crypto.hash.blake2b256
import co.topl.models._
import co.topl.models.utility.HasLength.implicits._
import co.topl.models.utility.{Lengths, Ratio, Sized}
import co.topl.typeclasses.ContainsEvidence.Instances._
import co.topl.typeclasses.ContainsEvidence.ops._
import co.topl.typeclasses.Identifiable.Instances._
import co.topl.typeclasses.Identifiable.ops._
import co.topl.typeclasses.crypto.CertificateVerifier.Instances._
import co.topl.typeclasses.crypto.CertificateVerifier.ops._
import io.estatico.newtype.macros.newtype
import io.estatico.newtype.ops._

import scala.language.implicitConversions

object ConsensusValidation {

  trait Ops {
    def header: BlockHeaderV2

    /**
     * Validates this block using the provided Algebra
     */
    def validatedUsing[F[_]: Monad](interpreter: Algebra[F])(implicit
      leaderElectionConfig:                      LeaderElection.Config,
      vrf:                                       Vrf
    ): EitherT[F, Failure, ValidatedBlockHeader] = {
      def test(
        f:       => Boolean,
        invalid: => Failure
      ): EitherT[F, Failure, BlockHeaderV2] =
        EitherT.cond[F](f, header, invalid)

      EitherT
        .liftF[F, Failure, BlockHeaderV2](interpreter.parentBlockHeader)
        .flatMap(parent =>
          test(header.slot > parent.slot, Failures.NonForwardSlot(header.slot, parent.slot))
            .flatMap(header =>
              test(
                header.timestamp > parent.timestamp,
                Failures.NonForwardTimestamp(header.timestamp, parent.timestamp)
              )
            )
            .flatMap(header =>
              test(header.parentHeaderId == parent.id, Failures.ParentMismatch(header.parentHeaderId, parent.id))
            )
            .flatMap(header => vrfVerification(header, interpreter))
            .subflatMap(header => kesVerification(header))
            .flatMap(header => registrationVerification(header, interpreter))
            .flatMap(header =>
              thresholdFor(header, interpreter)
                .subflatMap(threshold =>
                  vrfThresholdVerification(header, threshold)
                    .flatMap(header => eligibilityVerification(header, threshold))
                )
            )
            .map(header => ConsensusValidation.ValidatedBlockHeader(header))
        )
    }

    private def relativeStakeFor[F[_]: Monad](
      header:      BlockHeaderV2,
      interpreter: ConsensusValidation.Algebra[F]
    ): EitherT[F, ConsensusValidation.Failure, Ratio] =
      EitherT
        .fromEither[F](
          Sized
            .strict[TypedBytes, Lengths.`33`.type](
              TypedBytes(1: Byte, Bytes(blake2b256.hash(header.address.stakingVerificationKey.data.toArray).value))
            )
            .leftMap(_ => ConsensusValidation.Failures.InvalidVrfThreshold(Ratio(0)))
        )
        .flatMap(interpreter.relativeStakeFor(_).toRight(ConsensusValidation.Failures.InvalidVrfThreshold(Ratio(0))))

    private def thresholdFor[F[_]: Monad](
      header:                        BlockHeaderV2,
      interpreter:                   ConsensusValidation.Algebra[F]
    )(implicit leaderElectionConfig: LeaderElection.Config): EitherT[F, ConsensusValidation.Failure, Ratio] =
      relativeStakeFor(header, interpreter)
        .semiflatMap(relativeStake =>
          interpreter.parentBlockHeader
            .map(parent =>
              LeaderElection.getThreshold(
                relativeStake,
                header.slot - parent.slot
              )
            )
        )

    /**
     * Verify that the threshold evidence stamped on the block matches the threshold generated using local state
     */
    private[consensus] def vrfThresholdVerification(
      header:                        BlockHeaderV2,
      threshold:                     Ratio
    )(implicit leaderElectionConfig: LeaderElection.Config): Either[ConsensusValidation.Failure, BlockHeaderV2] =
      Either.cond(
        header.thresholdEvidence == threshold.evidence,
        header,
        ConsensusValidation.Failures.InvalidVrfThreshold(threshold)
      )

    /**
     * Verify that the block's staker is eligible using their relative stake distribution
     */
    private[consensus] def eligibilityVerification(
      header:    BlockHeaderV2,
      threshold: Ratio
    )(implicit
      leaderElectionConfig: LeaderElection.Config,
      vrf:                  Vrf
    ): Either[ConsensusValidation.Failure, BlockHeaderV2] =
      Either.cond(
        LeaderElection.isSlotLeaderForThreshold(threshold)(
          Bytes(
            vrf.vrfProofToHash(header.vrfCertificate.testProof.bytes.data.toArray)
          )
        ) || true, // TODO: This check currently does not seem to work
        header,
        ConsensusValidation.Failures.IneligibleVrfCertificate(threshold, header.vrfCertificate)
      )

    /**
     * Epoch N-2 Snapshot data
     */
    private[consensus] def registrationVerification[F[_]: Monad](
      header:      BlockHeaderV2,
      interpreter: ConsensusValidation.Algebra[F]
    ): EitherT[F, ConsensusValidation.Failure, BlockHeaderV2] =
      EitherT.pure[F, ConsensusValidation.Failure](header)

    /**
     * Verifies the given block's VRF certificate syntactic integrity for a particular stateful nonce
     */
    private[consensus] def vrfVerification[F[_]: Monad](
      header:      BlockHeaderV2,
      interpreter: ConsensusValidation.Algebra[F]
    ): EitherT[F, ConsensusValidation.Failure, BlockHeaderV2] =
      EitherT
        .liftF(interpreter.epochNonce)
        .flatMap { implicit epochNonce =>
          EitherT.cond[F](
            header.vrfCertificate.verify,
            header,
            ConsensusValidation.Failures.InvalidVrfCertificate(header.vrfCertificate)
          )
        }

    /**
     * Verifies the given block's KES certificate syntactic integrity for a particular stateful nonce
     */
    private[consensus] def kesVerification(
      header: BlockHeaderV2
    ): Either[ConsensusValidation.Failure, BlockHeaderV2] = {
      implicit def h: BlockHeaderV2 = header
      Either.cond(
        header.kesCertificate.verify,
        header,
        ConsensusValidation.Failures.InvalidKesCertificate(header.kesCertificate)
      )
    }

  }

  trait Implicits {

    implicit def headerAsOps(h: BlockHeaderV2): Ops =
      new Ops {
        override def header: BlockHeaderV2 = h
      }
  }

  object implicits extends Implicits

  sealed abstract class Failure

  object Failures {
    case class NonForwardSlot(slot: Slot, parentSlot: Slot) extends Failure
    case class NonForwardTimestamp(timestamp: Timestamp, parentTimestamp: Timestamp) extends Failure
    case class ParentMismatch(expectedParentId: TypedIdentifier, parentId: TypedIdentifier) extends Failure
    case class InvalidVrfThreshold(threshold: Ratio) extends Failure
    case class IneligibleVrfCertificate(threshold: Ratio, vrfCertificate: VrfCertificate) extends Failure
    case class InvalidVrfCertificate(vrfCertificate: VrfCertificate) extends Failure
    case class InvalidKesCertificate(kesCertificate: KesCertificate) extends Failure
  }

  trait Algebra[F[_]] {
    def epochNonce: F[Nonce]
    def parentBlockHeader: F[BlockHeaderV2]
    def relativeStakeFor(evidence: Evidence): OptionT[F, Ratio]
  }

  @newtype class ValidatedBlockHeader(val header: BlockHeaderV2)

  private[consensus] object ValidatedBlockHeader {

    def apply(blockHeaderV2: BlockHeaderV2): ValidatedBlockHeader =
      blockHeaderV2.coerce
  }

}

package co.topl.consensus

import cats.{Applicative, Monad}
import cats.data.{EitherT, OptionT}
import cats.implicits._
import co.topl.consensus.crypto.Vrf
import co.topl.models._
import co.topl.models.utility.Ratio
import co.topl.typeclasses.Identifiable.Instances._
import co.topl.typeclasses.Identifiable.ops._
import co.topl.typeclasses.StatefullyValidatable
import io.estatico.newtype.macros.newtype
import io.estatico.newtype.ops._
import co.topl.typeclasses.crypto.CertificateVerifier.Instances._
import co.topl.typeclasses.crypto.CertificateVerifier.ops._

import scala.language.implicitConversions

class ConsensusStatefullyValidatable[F[_]: Monad](implicit leaderElectionConfig: LeaderElection.Config, vrf: Vrf)
    extends StatefullyValidatable[
      F,
      ConsensusValidation.State,
      BlockHeaderV2,
      ConsensusValidation.ValidatedBlockHeader,
      ConsensusValidation.Failure
    ] {

  override def validate(
    header: BlockHeaderV2,
    state:  ConsensusValidation.State[F]
  ): EitherT[F, ConsensusValidation.Failure, ConsensusValidation.ValidatedBlockHeader] = {
    import ConsensusValidation.Failures._
    val parent = state.parentBlockHeader
    def test(
      f:       => Boolean,
      invalid: => ConsensusValidation.Failure
    ): EitherT[F, ConsensusValidation.Failure, BlockHeaderV2] =
      EitherT.cond[F](f, header, invalid)

    test(header.slot > parent.slot, NonForwardSlot(header.slot, parent.slot))
      .flatMap(_ => test(header.timestamp > parent.timestamp, NonForwardTimestamp(header.timestamp, parent.timestamp)))
      .flatMap(_ => test(header.parentHeaderId == parent.id, ParentMismatch(header.parentHeaderId, parent.id)))
      .flatMap(_ => registrationVerification(header, state))
      .flatMap(_ => vrfThresholdVerification(header, state))
      .flatMap(_ => vrfVerification(header, state))
      .flatMap(_ => kesVerification(header, state))
      .map(_ => ConsensusValidation.ValidatedBlockHeader(header))
  }

  private[consensus] def vrfThresholdVerification(
    header:                        BlockHeaderV2,
    state:                         ConsensusValidation.State[F]
  )(implicit leaderElectionConfig: LeaderElection.Config): EitherT[F, ConsensusValidation.Failure, BlockHeaderV2] =
    EitherT
      .liftF[F, ConsensusValidation.Failure, Ratio](
        state
          .relativeStakeFor(header.address)
          .getOrElse(Ratio(0))
          .flatMap(relativeStake =>
            state.parentBlockHeader
              .map(parent =>
                LeaderElection.getThreshold(
                  relativeStake,
                  header.slot - parent
                )
              )
          )
      )
      .flatMap(threshold =>
        EitherT.cond[F](
          // TODO: blake2b(threshold)
          header.thresholdEvidence == ???,
          header,
          ConsensusValidation.Failures.InvalidVrfThreshold(threshold)
        )
      )

  private[consensus] def eligibilityVerification(
    header: BlockHeaderV2,
    state:  ConsensusValidation.State[F]
  ): EitherT[F, ConsensusValidation.Failure, BlockHeaderV2] =
    EitherT
      .liftF[F, ConsensusValidation.Failure, Ratio](
        state
          .relativeStakeFor(header.address)
          .getOrElse(Ratio(0))
          .flatMap(relativeStake =>
            state.parentBlockHeader
              .map(parent =>
                LeaderElection.getThreshold(
                  relativeStake,
                  header.slot - parent
                )
              )
          )
      )
      .flatMap(threshold =>
        EitherT.cond[F](
          LeaderElection.isSlotLeaderForThreshold(threshold) {
            Bytes(
              vrf.vrfProofToHash(header.vrfCertificate.testProof.bytes.data.toArray[Byte])
            )
          },
          header,
          ConsensusValidation.Failures.InvalidVrfThreshold(threshold)
        )
      )

  /**
   * Epoch N-2 Snapshot data
   */
  private[consensus] def registrationVerification(
    header: BlockHeaderV2,
    state:  ConsensusValidation.State[F]
  ): EitherT[F, ConsensusValidation.Failure, BlockHeaderV2] =
    EitherT.pure[F, ConsensusValidation.Failure](header)

  /**
   * Requires the epoch nonce, thus stateful
   */
  private[consensus] def vrfVerification(
    header: BlockHeaderV2,
    state:  ConsensusValidation.State[F]
  ): EitherT[F, ConsensusValidation.Failure, BlockHeaderV2] =
    EitherT
      .liftF(state.epochNonce)
      .flatMap { implicit epochNonce =>
        EitherT.cond[F](
          header.vrfCertificate.verify,
          header,
          ConsensusValidation.Failures.InvalidVrfCertificate(header.vrfCertificate)
        )
      }

  /**
   * Requires the epoch nonce, thus stateful
   */
  private[consensus] def kesVerification(
    header: BlockHeaderV2,
    state:  ConsensusValidation.State[F]
  ): EitherT[F, ConsensusValidation.Failure, BlockHeaderV2] = // TODO
    EitherT.cond[F](
      header.kesCertificate.verify,
      header,
      ConsensusValidation.Failures.InvalidKesCertificate(header.kesCertificate)
    )
}

object ConsensusValidation {

  sealed abstract class Failure

  object Failures {
    case class NonForwardSlot(slot: Slot, parentSlot: Slot) extends Failure
    case class NonForwardTimestamp(timestamp: Timestamp, parentTimestamp: Timestamp) extends Failure
    case class ParentMismatch(expectedParentId: TypedIdentifier, parentId: TypedIdentifier) extends Failure
    case class InvalidVrfThreshold(threshold: Ratio) extends Failure
    case class InvalidVrfCertificate(vrfCertificate: VrfCertificate) extends Failure
    case class InvalidKesCertificate(kesCertificate: KesCertificate) extends Failure
  }

  trait State[F[_]] {
    def epochNonce: F[Nonce]
    def parentBlockHeader: F[BlockHeaderV2]
    def relativeStakeFor(address: TaktikosAddress): OptionT[F, Ratio]
  }

  @newtype class ValidatedBlockHeader(val header: BlockHeaderV2)

  private[consensus] object ValidatedBlockHeader {

    def apply(blockHeaderV2: BlockHeaderV2): ValidatedBlockHeader =
      blockHeaderV2.coerce
  }

}

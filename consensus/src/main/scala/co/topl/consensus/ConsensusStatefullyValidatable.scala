package co.topl.consensus

import co.topl.models._
import co.topl.typeclasses.Identifiable.Instances._
import co.topl.typeclasses.Identifiable.ops._
import co.topl.typeclasses.StatefullyValidatable
import io.estatico.newtype.macros.newtype
import io.estatico.newtype.ops._

class ConsensusStatefullyValidatable
    extends StatefullyValidatable[
      ConsensusValidation.State,
      BlockHeaderV2,
      ConsensusValidation.ValidatedBlockHeader,
      ConsensusValidation.Failure
    ] {

  override def validate(
    header: BlockHeaderV2,
    state:  ConsensusValidation.State
  ): Either[ConsensusValidation.Failure, ConsensusValidation.ValidatedBlockHeader] = {
    import ConsensusValidation.Failures._
    val parent = state.parentBlockHeader
    def test(f: => Boolean, invalid: => ConsensusValidation.Failure) =
      Either.cond(f, header, invalid)
    for {
      _ <- test(header.slot > parent.slot, NonForwardSlot(header.slot, parent.slot))
      _ <- test(header.timestamp > parent.timestamp, NonForwardTimestamp(header.timestamp, parent.timestamp))
      _ <- test(header.parentHeaderId == parent.id, ParentMismatch(header.parentHeaderId, parent.id))
      _ <- vrfVerification(header, state)
    } yield ConsensusValidation.ValidatedBlockHeader(header)
  }

  private[consensus] def vrfVerification(
    header: BlockHeaderV2,
    state:  ConsensusValidation.State
  ): Either[ConsensusValidation.Failure, BlockHeaderV2] = // TODO
    Either.cond(true, header, ConsensusValidation.Failures.InvalidVrfCertificate(header.vrfCertificate))
}

object ConsensusValidation {

  implicit val instance: ConsensusStatefullyValidatable = new ConsensusStatefullyValidatable

  sealed abstract class Failure

  object Failures {
    case class NonForwardSlot(slot: Slot, parentSlot: Slot) extends Failure
    case class NonForwardTimestamp(timestamp: Timestamp, parentTimestamp: Timestamp) extends Failure
    case class ParentMismatch(expectedParentId: TypedIdentifier, parentId: TypedIdentifier) extends Failure
    case class InvalidVrfCertificate(vrfCertificate: VrfCertificate) extends Failure
  }

  trait State {
    def epochNonce: Nonce
    def totalStake: Int128
    def parentBlockHeader: BlockHeaderV2
    def stakeFor(address: Address): Option[Int128]
  }

  @newtype class ValidatedBlockHeader(val header: BlockHeaderV2)

  private[consensus] object ValidatedBlockHeader {

    def apply(blockHeaderV2: BlockHeaderV2): ValidatedBlockHeader =
      blockHeaderV2.coerce
  }

}

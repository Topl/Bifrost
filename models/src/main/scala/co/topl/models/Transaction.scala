package co.topl.models

import cats.data.Chain
import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.models.utility.{Lengths, Sized}

case class Transaction( // TODO it should be deleted and use import co.topl.proto.models.Transaction
  inputs:   Chain[Transaction.Input],
  outputs:  Chain[Transaction.Output],
  schedule: Transaction.Schedule,
  data:     Option[Transaction.DataTetra]
)

object Transaction {

  // Used by Dion protocol
  type Data = Sized.Max[Latin1Data, Lengths.`127`.type]
  // Used by Tetra Protocol
  type DataTetra = Bytes
  val MaxDataLength = 15360

  case class Input(
    boxId:       Box.Id,
    proposition: Proposition,
    proof:       Proof,
    value:       Box.Value
  )
  case class Output(address: FullAddress, value: Box.Value, minting: Boolean)

  /**
   * Represents events in time for a particular transaction
   * @param creation When did the user create the transaction?
   * @param minimumSlot What is the earliest slot in which this transaction can be included in the blockchain?
   * @param maximumSlot What is the latest slot in which this transaction can be included in the blockchain?
   */
  case class Schedule(creation: Timestamp, minimumSlot: Slot, maximumSlot: Slot)

  case class Unproven( // TODO Remove after full model replacement
    inputs:   Chain[Transaction.Unproven.Input],
    outputs:  Chain[Transaction.Output],
    schedule: Schedule,
    data:     Option[Transaction.DataTetra]
  )

  case class UnprovenProto(
    inputs:   Chain[Transaction.Unproven.InputProto],
    outputs:  Chain[co.topl.proto.models.Transaction.UnspentOutput],
    schedule: Option[co.topl.proto.models.Transaction.Schedule],
    data:     com.google.protobuf.ByteString
  )

  object Unproven {

    case class Input( // TODO Remove after full model replacement
      boxId:       Box.Id,
      proposition: Proposition,
      value:       Box.Value
    )

    /**
     * Proof is not included
     */
    case class InputProto(
      boxId:       Option[co.topl.proto.models.Box.Id],
      proposition: co.topl.proto.models.Proposition,
      value:       co.topl.proto.models.BoxValue
    )
  }

}

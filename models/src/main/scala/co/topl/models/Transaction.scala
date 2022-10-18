package co.topl.models

import cats.data.Chain

case class Transaction(
  inputs:   Chain[Transaction.Input],
  outputs:  Chain[Transaction.Output],
  schedule: Transaction.Schedule,
  data:     Option[Transaction.Data]
)

object Transaction {

  type Data = Bytes
  val maxDataLength = 15360

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

  case class Unproven(
    inputs:   Chain[Transaction.Unproven.Input],
    outputs:  Chain[Transaction.Output],
    schedule: Schedule,
    data:     Option[Transaction.Data]
  )

  object Unproven {

    case class Input(
      boxId:       Box.Id,
      proposition: Proposition,
      value:       Box.Value
    )
  }

}

package co.topl.models

import cats.data.Chain
import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.models.utility.{Lengths, Sized}

case class Transaction(
  inputs:     Chain[Transaction.Input],
  outputs:    Chain[Transaction.Output],
  chronology: Transaction.Chronology,
  data:       Option[Transaction.Data]
)

object Transaction {

  type Data = Sized.Max[Latin1Data, Lengths.`127`.type]

  /**
   * @param transactionOutputIndex TODO: How does the network behave if we allow a huge number of outputs in a transaction?
   */
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
  case class Chronology(creation: Timestamp, minimumSlot: Slot, maximumSlot: Slot)

  case class Unproven(
    inputs:     Chain[Transaction.Unproven.Input],
    outputs:    Chain[Transaction.Output],
    chronology: Chronology,
    data:       Option[Transaction.Data]
  )

  object Unproven {

    case class Input(
      boxId:       Box.Id,
      proposition: Proposition,
      value:       Box.Value
    )
  }

}

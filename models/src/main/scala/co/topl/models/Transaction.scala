package co.topl.models

import cats.data.Chain

case class Transaction(
  inputs:     Chain[Transaction.Input],
  outputs:    Chain[Transaction.Output],
  chronology: Transaction.Chronology,
  data:       Option[TransactionData]
)

object Transaction {

  /**
   * @param transactionOutputIndex TODO: How does the network behave if we allow a huge number of outputs in a transaction?
   */
  case class Input(
    transactionId:          TypedIdentifier,
    transactionOutputIndex: Short,
    proposition:            Proposition,
    proof:                  Proof,
    value:                  Box.Value
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
    data:       Option[TransactionData]
  )

  object Unproven {

    case class Input(
      transactionId:          TypedIdentifier,
      transactionOutputIndex: Short,
      proposition:            Proposition,
      value:                  Box.Value
    )
  }

}

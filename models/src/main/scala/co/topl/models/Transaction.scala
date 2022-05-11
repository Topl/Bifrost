package co.topl.models

import cats.data.{Chain, NonEmptyChain}

case class Transaction(
  inputs:    Chain[Transaction.Input],
  outputs:   NonEmptyChain[Transaction.Output],
  timestamp: Timestamp,
  data:      Option[TransactionData]
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
  case class Output(dionAddress: DionAddress, value: Box.Value, minting: Boolean)

  case class Unproven(
    inputs:    Chain[Transaction.Unproven.Input],
    outputs:   NonEmptyChain[Transaction.Output],
    timestamp: Timestamp,
    data:      Option[TransactionData]
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

package co.topl.models

import cats.data.NonEmptyChain

import scala.collection.immutable.ListMap

case class Transaction(
  inputs:    NonEmptyChain[Transaction.Input],
  outputs:   NonEmptyChain[Transaction.Output],
  timestamp: Timestamp,
  data:      Option[TransactionData]
)

object Transaction {

  case class Input(
    transactionId:          TypedIdentifier,
    transactionOutputIndex: Short,
    proposition:            Proposition,
    proof:                  Proof,
    value:                  Box.Value
  )
  case class Output(dionAddress: DionAddress, value: Box.Value, minting: Boolean)

  case class Unproven(
    inputs:    NonEmptyChain[Transaction.Unproven.Input],
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

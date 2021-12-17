package co.topl.models

import cats.data.NonEmptyChain

import scala.collection.immutable.ListMap

case class Transaction(
  inputs:      ListMap[BoxReference, (Proposition, Proof)],
  feeOutput:   Option[Transaction.PolyOutput],
  coinOutputs: NonEmptyChain[Transaction.CoinOutput],
  fee:         Int128,
  timestamp:   Timestamp,
  data:        Option[TransactionData],
  minting:     Boolean
)

// TODO: Syntactic and Semantic Validation
object Transaction {
  sealed abstract class CoinOutput
  case class PolyOutput(dionAddress: DionAddress, value: Int128) extends CoinOutput
  case class ArbitOutput(dionAddress: DionAddress, value: Int128) extends CoinOutput
  case class AssetOutput(dionAddress: DionAddress, value: Box.Values.Asset) extends CoinOutput

  case class Unproven(
    inputs:      List[BoxReference],
    feeOutput:   Option[Transaction.PolyOutput],
    coinOutputs: NonEmptyChain[Transaction.CoinOutput],
    fee:         Int128,
    timestamp:   Timestamp,
    data:        Option[TransactionData],
    minting:     Boolean
  )

  object Unproven {

    def apply(transaction: Transaction): Unproven =
      Unproven(
        transaction.inputs.keys.toList,
        transaction.feeOutput,
        transaction.coinOutputs,
        transaction.fee,
        transaction.timestamp,
        transaction.data,
        transaction.minting
      )
  }

}

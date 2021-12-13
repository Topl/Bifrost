package co.topl.models

import cats.data.NonEmptyChain

import scala.collection.immutable.ListMap

case class Transaction(
  inputs:      ListMap[BoxReference, (Proposition, Proof)],
  feeOutput:   Option[Transaction.PolyOutput],
  coinOutputs: NonEmptyChain[Transaction.CoinOutput[_]],
  fee:         Int128,
  timestamp:   Timestamp,
  data:        Option[TransactionData],
  minting:     Boolean
)

// TODO: Syntactic and Semantic Validation
object Transaction {
  sealed abstract class CoinOutput[+T](val dionAddress: DionAddress, val value: T)
  case class PolyOutput(override val dionAddress: DionAddress, override val value: Int128) extends CoinOutput[Int128](dionAddress, value)
  case class ArbitOutput(override val dionAddress: DionAddress, override val value: Int128) extends CoinOutput[Int128](dionAddress, value)
  case class AssetOutput(override val dionAddress: DionAddress, override val value: Box.Values.Asset) extends CoinOutput[Box.Values.Asset](dionAddress, value)

  case class Unproven(
    inputs:      List[BoxReference],
    feeOutput:   Option[Transaction.PolyOutput],
    coinOutputs: NonEmptyChain[Transaction.CoinOutput[_]],
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

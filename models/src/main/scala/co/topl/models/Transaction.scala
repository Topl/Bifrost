package co.topl.models

import cats.data.NonEmptyChain

case class Transaction(
  input:       Seq[BoxReference],
  feeOutput:   Option[Transaction.PolyOutput],
  coinOutputs: NonEmptyChain[Transaction.CoinOutput],
  attestation: Attestation,
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
}

package co.topl.models

import cats.data.NonEmptyChain

/**
 * Represents an agreement to spend boxes to create new boxes
 */
sealed abstract class Transaction

object Transactions {

  case class Poly(
    input:       Seq[BoxReference],
    feeOutput:   Option[PolyOutput],
    coinOutputs: NonEmptyChain[PolyOutput],
    attestation: Attestation,
    fee:         Int128,
    timestamp:   Timestamp,
    data:        Option[TransactionData],
    minting:     Boolean
  ) extends Transaction

  case class Arbit(
    input:       Seq[BoxReference],
    feeOutput:   PolyOutput,
    coinOutputs: NonEmptyChain[ArbitOutput],
    attestation: Attestation,
    fee:         Int128,
    timestamp:   Timestamp,
    data:        Option[TransactionData],
    minting:     Boolean
  ) extends Transaction

  case class Asset(
    input:       Seq[BoxReference],
    feeOutput:   PolyOutput,
    coinOutputs: NonEmptyChain[AssetOutput],
    attestation: Attestation,
    fee:         Int128,
    timestamp:   Timestamp,
    data:        Option[TransactionData],
    minting:     Boolean
  ) extends Transaction
}

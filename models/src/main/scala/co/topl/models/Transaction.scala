package co.topl.models

import cats.data.NonEmptyChain

/**
 * Represents an attested/proven transaction
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

/**
 * Represents a transaction without any attestation information
 */
sealed abstract class UnprovenTransaction

object UnprovenTransactions {

  case class Poly(
    input:       Seq[BoxReference],
    feeOutput:   Option[PolyOutput],
    coinOutputs: NonEmptyChain[PolyOutput],
    fee:         Int128,
    timestamp:   Timestamp,
    data:        Option[TransactionData],
    minting:     Boolean
  ) extends UnprovenTransaction

  case class Arbit(
    input:       Seq[BoxReference],
    feeOutput:   PolyOutput,
    coinOutputs: NonEmptyChain[ArbitOutput],
    fee:         Int128,
    timestamp:   Timestamp,
    data:        Option[TransactionData],
    minting:     Boolean
  ) extends UnprovenTransaction

  case class Asset(
    input:       Seq[BoxReference],
    feeOutput:   PolyOutput,
    coinOutputs: NonEmptyChain[AssetOutput],
    fee:         Int128,
    timestamp:   Timestamp,
    data:        Option[TransactionData],
    minting:     Boolean
  ) extends UnprovenTransaction
}

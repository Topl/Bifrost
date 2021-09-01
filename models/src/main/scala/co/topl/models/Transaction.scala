package co.topl.models

import cats.data.NonEmptyChain
import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.models.utility.{Lengths, Sized}

sealed abstract class Transaction

case class PolyTransfer(
  input:       Seq[BoxReference],
  feeOutput:   Option[PolyOutput],
  coinOutputs: NonEmptyChain[PolyOutput],
  attestation: Attestation,
  fee:         Int128,
  timestamp:   Timestamp,
  data:        Option[Sized.Max[Latin1Data, Lengths.`127`.type]],
  minting:     Boolean
) extends Transaction

case class ArbitTransfer(
  input:       Seq[BoxReference],
  feeOutput:   PolyOutput,
  coinOutputs: NonEmptyChain[ArbitOutput],
  attestation: Attestation,
  fee:         Int128,
  timestamp:   Timestamp,
  data:        Option[Sized.Max[Latin1Data, Lengths.`127`.type]],
  minting:     Boolean
) extends Transaction

case class AssetTransfer(
  input:       Seq[BoxReference],
  feeOutput:   PolyOutput,
  coinOutputs: NonEmptyChain[AssetOutput],
  attestation: Attestation,
  fee:         Int128,
  timestamp:   Timestamp,
  data:        Option[Sized.Max[Latin1Data, Lengths.`127`.type]],
  minting:     Boolean
) extends Transaction

// Not needed for phase 0
case class TetraTransfer(
  arbitBoxRef:           BoxReference,
  registrationBoxRef:    Box[Box.Values.TaktikosRegistration],
  feeOutput:             PolyOutput,
  outputTaktikosBoxData: (TaktikosAddress, Int128, Registration, Signature)
) extends Transaction

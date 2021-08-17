package co.topl.models

import cats.data.NonEmptyChain
import co.topl.models.StringDataTypes.Latin1Data

sealed abstract class Transaction

case class PolyTransfer(
  input:       Seq[BoxReference],
  feeOutput:   PolyOutput,
  polyOutputs: NonEmptyChain[PolyOutput],
  attestation: Attestation,
  fee:         Int128,
  timestamp:   Timestamp,
  data:        Option[Sized.Max[Latin1Data, Lengths.`127`.type]],
  minting:     Boolean
) extends Transaction

case class ArbitTransfer(
  input:        Seq[BoxReference],
  feeOutput:    PolyOutput,
  arbitOutputs: NonEmptyChain[ArbitOutput],
  attestation:  Attestation,
  fee:          Int128,
  timestamp:    Timestamp,
  data:         Option[Sized.Max[Latin1Data, Lengths.`127`.type]],
  minting:      Boolean
) extends Transaction

case class AssetTransfer(
  input:        Seq[BoxReference],
  feeOutput:    PolyOutput,
  assetOutputs: NonEmptyChain[AssetOutput],
  attestation:  Attestation,
  fee:          Int128,
  timestamp:    Timestamp,
  data:         Option[Sized.Max[Latin1Data, Lengths.`127`.type]],
  minting:      Boolean
) extends Transaction

case class TetraTransfer(
  arbitBoxRef:           BoxReference,
  registrationBoxRef:    TaktikosBoxReference,
  feeOutput:             PolyOutput,
  outputTaktikosBoxData: (TaktikosAddress, Int128, Registration, Signature)
) extends Transaction

case class TetraRegistration(
  polyBoxRef:                BoxReference,
  taktikosBoxRef:            TaktikosBoxReference,
  feeOutput:                 PolyOutput,
  outputRegistrationBoxData: (Registration, Signature)
) extends Transaction

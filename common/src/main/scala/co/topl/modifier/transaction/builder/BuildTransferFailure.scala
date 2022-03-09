package co.topl.modifier.transaction.builder

import co.topl.attestation.Address
import co.topl.modifier.box.AssetCode
import co.topl.utils.Int128
import co.topl.utils.StringDataTypes.Latin1Data

sealed trait BuildTransferFailure

object BuildTransferFailures {
  case object DuplicateInputs extends BuildTransferFailure

  case object EmptyOutputs extends BuildTransferFailure

  case object DuplicateOutputs extends BuildTransferFailure

  case class InvalidOutputValues(values: List[Int128]) extends BuildTransferFailure

  case class InsufficientPolyFunds(provided: Int128, required: Int128) extends BuildTransferFailure

  case class InsufficientArbitFunds(provided: Int128, required: Int128) extends BuildTransferFailure

  case class InsufficientAssetFunds(assetCode: AssetCode, provided: Int128, required: Int128)
      extends BuildTransferFailure

  case class MultipleAssetCodes(expected: AssetCode, found: List[AssetCode]) extends BuildTransferFailure

  case class Int128Overflow(value: Int128) extends BuildTransferFailure

  case class InvalidAddress(address: Address) extends BuildTransferFailure

  case class InvalidShortName(shortName: Latin1Data) extends BuildTransferFailure
}

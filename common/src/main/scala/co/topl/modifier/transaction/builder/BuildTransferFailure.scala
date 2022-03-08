package co.topl.modifier.transaction.builder

import cats.Show
import co.topl.attestation.Address
import co.topl.attestation.ops.AddressOps.ToDionAddressFailure
import co.topl.models.Int128
import co.topl.utils.StringDataTypes.Latin1Data

sealed trait BuildTransferFailure

object BuildTransferFailures {
  case object DuplicateInputs extends BuildTransferFailure
  case object EmptyInputs extends BuildTransferFailure
  case object EmptyOutputs extends BuildTransferFailure
  case object DuplicateOutputs extends BuildTransferFailure
  case object InsufficientFeeFunds extends BuildTransferFailure
  case object InsufficientPaymentFunds extends BuildTransferFailure
  case object DifferentInputOutputCodes extends BuildTransferFailure
  case object DuplicateAssetCodes extends BuildTransferFailure
  case class InvalidInputBox(message: String) extends BuildTransferFailure
  case class Int128Overflow(value: Int128) extends BuildTransferFailure
  case class InvalidAddress(address: Address) extends BuildTransferFailure
  case class InvalidShortName(shortName: Latin1Data) extends BuildTransferFailure
}

object BuildTransferFailure {

  trait Implicits {

    implicit val buildTransferFailureShow: Show[BuildTransferFailure] = { transfer =>
      val failureMessage = "Failed to build unsigned transfer: "

      val specificMessage = transfer match {
        case BuildTransferFailures.DuplicateInputs =>
          "duplicate input boxes provided"
        case BuildTransferFailures.EmptyInputs =>
          "no input boxes provided"
        case BuildTransferFailures.EmptyOutputs =>
          "no transfer recipients provided"
        case BuildTransferFailures.DuplicateOutputs =>
          "duplicate transfer recipients provided"
        case BuildTransferFailures.InsufficientFeeFunds =>
          "insufficient poly funds provided to pay fee"
        case BuildTransferFailures.InsufficientPaymentFunds =>
          "insufficient token funds provided to pay recipients"
        case BuildTransferFailures.DifferentInputOutputCodes =>
          "input asset codes are different than output asset codes"
        case BuildTransferFailures.DuplicateAssetCodes =>
          "duplicate input asset codes provided"
      }

      failureMessage + specificMessage
    }
  }

  object implicits extends Implicits
}

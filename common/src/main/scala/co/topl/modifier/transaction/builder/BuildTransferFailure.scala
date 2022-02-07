package co.topl.modifier.transaction.builder

import cats.Show

sealed trait BuildTransferFailure

object BuildTransferFailures {
  case object DuplicateInputs extends BuildTransferFailure
  case object EmptyInputs extends BuildTransferFailure
  case object EmptyRecipients extends BuildTransferFailure
  case object DuplicateRecipients extends BuildTransferFailure
  case object InsufficientFeeFunds extends BuildTransferFailure
  case object InsufficientPaymentFunds extends BuildTransferFailure
  case object DifferentInputOutputCodes extends BuildTransferFailure
  case object DuplicateAssetCodes extends BuildTransferFailure
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
        case BuildTransferFailures.EmptyRecipients =>
          "no transfer recipients provided"
        case BuildTransferFailures.DuplicateRecipients =>
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

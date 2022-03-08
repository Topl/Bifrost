package co.topl.modifier.transaction.builder

import co.topl.attestation.Address
import co.topl.modifier.box.{AssetBox, AssetCode, Box}
import co.topl.utils.Int128

object Validation {

  def validateNonEmptyInputNonces(
    inputs: List[Box.Nonce]
  ): Either[BuildTransferFailure, List[Box.Nonce]] =
    Either.cond(inputs.nonEmpty, inputs, BuildTransferFailures.EmptyInputs)

  def validateUniqueInputNonces(
    inputs: List[Box.Nonce]
  ): Either[BuildTransferFailure, List[Box.Nonce]] =
    Either.cond(
      inputs.distinct.length == inputs.length,
      inputs,
      BuildTransferFailures.DuplicateInputs
    )

  def validateNonEmptyOutputAddresses(
    outputs: List[Address]
  ): Either[BuildTransferFailure, List[Address]] =
    Either.cond(outputs.nonEmpty, outputs, BuildTransferFailures.EmptyOutputs)

  def validateUniqueOutputAddresses(
    outputs: List[Address]
  ): Either[BuildTransferFailure, List[Address]] =
    Either.cond(outputs.distinct.length == outputs.length, outputs, BuildTransferFailures.DuplicateOutputs)

  /**
   * Validates that the given funds can pay the given fee.
   * @param funds the funds to validate
   * @param feeAmount the fee amount to pay
   * @return the valid funds, otherwise a `BuildTransferFailures.InsufficientFeeFunds` failure
   */
  def validateFeeFunds(funds: Int128, feeAmount: Int128): Either[BuildTransferFailure, Int128] =
    Either.cond(
      funds >= feeAmount,
      funds,
      BuildTransferFailures.InsufficientFeeFunds
    )

  /**
   * Validates that the given funds can pay the given amount.
   * @param funds the funds to validate
   * @param paymentAmount the amount required to pay
   * @return the valid funds, otherwise a `BuildTransferFailures.InsufficientPaymentFunds`
   */
  def validatePaymentFunds(funds: Int128, paymentAmount: Int128): Either[BuildTransferFailure, Int128] =
    Either.cond(
      funds >= paymentAmount,
      funds,
      BuildTransferFailures.InsufficientPaymentFunds
    )

  /**
   * Validates that the asset codes of the given boxes match the given asset code.
   * @param expectedAssetCode the asset code expected in each box
   * @param assetBoxes the list of boxes to validate
   * @param minting whether or not the asset transfer is minting
   * @return the asset code if successful, otherwise a `BuildTransferFailures.DifferentInputOutputCodes` failure
   */
  def validateSameAssetCode(
    expectedAssetCode: AssetCode,
    assetBoxes:        List[AssetBox]
  ): Either[BuildTransferFailure, AssetCode] =
    Either.cond(
      assetBoxes.forall(box => box.value.assetCode == expectedAssetCode),
      expectedAssetCode,
      BuildTransferFailures.DifferentInputOutputCodes
    )
}

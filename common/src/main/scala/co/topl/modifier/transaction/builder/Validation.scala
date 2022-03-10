package co.topl.modifier.transaction.builder

import co.topl.attestation.Address
import co.topl.modifier.box.{AssetBox, AssetCode, Box}
import co.topl.utils.Int128
import cats.implicits._
import co.topl.models.Transaction

object Validation {

  def validateNonEmptyPolyInputNonces(
    polyInputs: List[Box.Nonce]
  ): Either[BuildTransferFailure, List[Box.Nonce]] =
    Either.cond(polyInputs.nonEmpty, polyInputs, BuildTransferFailures.EmptyPolyInputs)

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

  def validatePositiveOutputValues(
    values: List[Int128]
  ): Either[BuildTransferFailure, List[Int128]] = {
    val nonPositiveValues = values.filter(_ <= 0)
    Either.cond(nonPositiveValues.isEmpty, values, BuildTransferFailures.InvalidOutputValues(nonPositiveValues))
  }

  def validatePolyFunds(funds: Int128, feeAmount: Int128, paymentAmount: Int128): Either[BuildTransferFailure, Int128] =
    Either.cond(
      funds >= feeAmount + paymentAmount,
      funds,
      BuildTransferFailures.InsufficientPolyFunds(funds, feeAmount + paymentAmount)
    )

  def validateArbitFunds(
    funds:         Int128,
    paymentAmount: Int128
  ): Either[BuildTransferFailure, Int128] =
    Either.cond(
      funds >= paymentAmount,
      funds,
      BuildTransferFailures.InsufficientArbitFunds(funds, paymentAmount)
    )

  def validateAssetFunds(
    funds:          Map[AssetCode, Int128],
    paymentAmounts: Map[AssetCode, Int128]
  ): Either[BuildTransferFailure, Map[AssetCode, Int128]] =
    paymentAmounts.toList
      .traverse { asset =>
        val assetFunds = funds.getOrElse(asset._1, Int128(0))
        Either.cond(
          assetFunds >= asset._2,
          asset,
          BuildTransferFailures.InsufficientAssetFunds(asset._1, assetFunds, asset._2)
        )
      }
      .map(_.toMap)

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
  ): Either[BuildTransferFailure, AssetCode] = {
    val nonMatchingAssetCodes = assetBoxes.map(_.value.assetCode).filter(_ != expectedAssetCode)

    Either.cond(
      nonMatchingAssetCodes.isEmpty,
      expectedAssetCode,
      BuildTransferFailures.MultipleAssetCodes(expectedAssetCode, nonMatchingAssetCodes)
    )
  }
}

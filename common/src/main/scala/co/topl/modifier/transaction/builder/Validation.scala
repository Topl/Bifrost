package co.topl.modifier.transaction.builder

import cats.implicits._
import co.topl.attestation.Address
import co.topl.modifier.box.{AssetBox, AssetCode, TokenBox, TokenValueHolder}
import co.topl.utils.Int128

object Validation {

  /**
   * Validates that a given list of boxes is non-empty.
   * Fails with `BuildTransferFailures.EmptyInputs`.
   * @param boxes the list of boxes to check
   * @tparam A the type of TokenValueHolder contained by the boxes
   * @tparam B the type of the boxes
   * @return the input boxes if successful, otherwise a `BuildTransferFailures.EmptyInputs` failure
   */
  def validateNonEmptyInputs[A <: TokenValueHolder, B <: TokenBox[A]](
    boxes: List[(Address, B)]
  ): Either[BuildTransferFailure, List[(Address, B)]] =
    Either.cond(boxes.nonEmpty, boxes, BuildTransferFailures.EmptyInputs)

  /**
   * Validates that a given list of boxes contains only unique inputs.
   * Fails with `BuildTransferFailures.DuplicateInputs`.
   * @param boxes the list of boxes to validate
   * @tparam A the type of TokenValueHolder contained by the boxes
   * @tparam B the type of the boxes
   * @return the input boxes if successful, otherwise a `BuildTransferFailures.DuplicateInputs` failure
   */
  def validateUniqueInputs[A <: TokenValueHolder, B <: TokenBox[A]](
    boxes: List[(Address, B)]
  ): Either[BuildTransferFailure, List[(Address, B)]] =
    Either.cond(boxes.map(_._2.nonce).distinct.length == boxes.length, boxes, BuildTransferFailures.DuplicateInputs)

  /**
   * Validates that a list of recipients is non empty
   * Fails with `BuildTransferFailures.EmptyRecipients`.
   * @param recipients the list of recipients to validate
   * @return the recipients if successful, otherwise a `BuildTransferFailures.EmptyRecipients` failure
   */
  def validateNonEmptyRecipients(
    recipients: List[(Address, _)]
  ): Either[BuildTransferFailure, List[(Address, _)]] =
    Either.cond(recipients.nonEmpty, recipients, BuildTransferFailures.EmptyRecipients)

  /**
   * Validates that the given recipients are all unique values.
   * Fails with `BuildTransferFailures.DuplicateRecipients`.
   * @param recipients the list of recipients to validate
   * @return the recipients if successful, otherwise a `BuildTransferFailures.DuplicateRecipients` failure
   */
  def validateUniqueRecipients(
    recipients: List[(Address, _)]
  ): Either[BuildTransferFailure, List[(Address, _)]] =
    Either.cond(
      recipients.map(_._1).distinct.length == recipients.length,
      recipients,
      BuildTransferFailures.DuplicateRecipients
    )

  /**
   * Validates that the given funds can pay the given fee.
   * @param funds the funds to validate
   * @param feeAmount the fee amount to pay
   * @return the remainder after deducting the fee if successful,
   *         otherwise a `BuildTransferFailures.InsufficientFeeFunds` failure
   */
  def validateFeeFunds(funds: Int128, feeAmount: Int128): Either[BuildTransferFailure, Int128] =
    Either.cond(funds >= feeAmount, funds - feeAmount, BuildTransferFailures.InsufficientFeeFunds)

  /**
   * Validates that the given funds can pay the given amount.
   * @param funds the funds to validate
   * @param paymentAmount the amount required to pay
   * @return the remainder after deducting the payment amount if successful,
   *         otherwise a `BuildTransferFailures.InsufficientPaymentFunds`
   */
  def validatePaymentFunds(funds: Int128, paymentAmount: Int128): Either[BuildTransferFailure, Int128] =
    Either.cond(funds >= paymentAmount, funds - paymentAmount, BuildTransferFailures.InsufficientPaymentFunds)

  /**
   * Validates that the given list of asset boxes is non-empty, distinct, and has matching asset codes.
   * @param assetBoxes the list of asset boxes to validate
   * @param minting whether or not the asset transfer is minting
   * @return the input list of asset boxes if successful, otherwise a `BuildTransferFailures.DuplicateAssetCodes` failure
   */
  def validateAssetInputs(
    assetBoxes: List[(Address, AssetBox)],
    minting:    Boolean
  ): Either[BuildTransferFailure, List[(Address, AssetBox)]] =
    if (!minting)
      for {
        _ <- Either.cond(assetBoxes.nonEmpty, assetBoxes, BuildTransferFailures.EmptyInputs)
        _ <- Either.cond(
          assetBoxes.map(_._2.nonce).distinct.length == assetBoxes.length,
          assetBoxes,
          BuildTransferFailures.DuplicateInputs
        )
        _ <- Either.cond(
          assetBoxes.map(_._2.value.assetCode).distinct.length == 1,
          assetBoxes,
          BuildTransferFailures.DuplicateAssetCodes
        )
      } yield assetBoxes
    else assetBoxes.asRight

  /**
   * Validates that the asset codes of the given boxes match the given asset code.
   * @param expectedAssetCode the asset code expected in each box
   * @param assetBoxes the list of boxes to validate
   * @param minting whether or not the asset transfer is minting
   * @return the asset code if successful, otherwise a `BuildTransferFailures.DifferentInputOutputCodes` failure
   */
  def validateSameAssetCode(
    expectedAssetCode: AssetCode,
    assetBoxes:        List[(Address, AssetBox)],
    minting:           Boolean
  ): Either[BuildTransferFailure, AssetCode] =
    if (!minting)
      Either.cond(
        assetBoxes.head._2.value.assetCode == expectedAssetCode,
        expectedAssetCode,
        BuildTransferFailures.DifferentInputOutputCodes
      )
    else
      expectedAssetCode.asRight
}

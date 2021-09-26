package co.topl.modifier.transaction.builder

import cats.implicits._
import co.topl.attestation.Address
import co.topl.modifier.box.{AssetBox, AssetCode, TokenBox, TokenValueHolder}
import co.topl.utils.Int128

object Validation {

  def validateNonEmptyInputs[A <: TokenValueHolder, B <: TokenBox[A]](
    boxes: List[(Address, B)]
  ): Either[BuildTransferFailure, List[(Address, B)]] =
    Either.cond(boxes.nonEmpty, boxes, BuildTransferFailures.EmptyInputs)

  def validateUniqueInputs[A <: TokenValueHolder, B <: TokenBox[A]](
    boxes: List[(Address, B)]
  ): Either[BuildTransferFailure, List[(Address, B)]] =
    Either.cond(boxes.map(_._2.nonce).distinct.length == boxes.length, boxes, BuildTransferFailures.DuplicateInputs)

  def validateNonEmptyRecipients(
    recipients: List[(Address, _)]
  ): Either[BuildTransferFailure, List[(Address, _)]] =
    Either.cond(recipients.nonEmpty, recipients, BuildTransferFailures.EmptyRecipients)

  def validateUniqueRecipients(
    recipients: List[(Address, _)]
  ): Either[BuildTransferFailure, List[(Address, _)]] =
    Either.cond(
      recipients.map(_._1).distinct.length == recipients.length,
      recipients,
      BuildTransferFailures.DuplicateRecipients
    )

  def validateFeeFunds(funds: Int128, feeAmount: Int128): Either[BuildTransferFailure, Int128] =
    Either.cond(funds >= feeAmount, funds - feeAmount, BuildTransferFailures.InsufficientFeeFunds)

  def validatePaymentFunds(funds: Int128, paymentAmount: Int128): Either[BuildTransferFailure, Int128] =
    Either.cond(funds >= paymentAmount, funds - paymentAmount, BuildTransferFailures.InsufficientPaymentFunds)

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

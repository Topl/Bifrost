package co.topl.modifier.transaction.unsigned

import cats.implicits._
import co.topl.attestation.Address
import co.topl.modifier.box.{AssetBox, AssetCode, TokenBox, TokenValueHolder}
import co.topl.utils.Int128

object Validation {

  def validateNonEmptyInputs[A <: TokenValueHolder, B <: TokenBox[A]](
    boxes: List[(Address, B)]
  ): Either[UnsignedTransferFailure, List[(Address, B)]] =
    Either.cond(boxes.nonEmpty, boxes, UnsignedTransferFailures.EmptyInputs)

  def validateUniqueInputs[A <: TokenValueHolder, B <: TokenBox[A]](
    boxes: List[(Address, B)]
  ): Either[UnsignedTransferFailure, List[(Address, B)]] =
    Either.cond(boxes.map(_._2.nonce).distinct.length == boxes.length, boxes, UnsignedTransferFailures.DuplicateInputs)

  def validateNonEmptyRecipients(
    recipients: List[(Address, _)]
  ): Either[UnsignedTransferFailure, List[(Address, _)]] =
    Either.cond(recipients.nonEmpty, recipients, UnsignedTransferFailures.EmptyRecipients)

  def validateUniqueRecipients(
    recipients: List[(Address, _)]
  ): Either[UnsignedTransferFailure, List[(Address, _)]] =
    Either.cond(
      recipients.map(_._1).distinct.length == recipients.length,
      recipients,
      UnsignedTransferFailures.DuplicateRecipients
    )

  def validateFeeFunds(funds: Int128, feeAmount: Int128): Either[UnsignedTransferFailure, Int128] =
    Either.cond(funds >= feeAmount, funds - feeAmount, UnsignedTransferFailures.InsufficientFeeFunds)

  def validatePaymentFunds(funds: Int128, paymentAmount: Int128): Either[UnsignedTransferFailure, Int128] =
    Either.cond(funds >= paymentAmount, funds - paymentAmount, UnsignedTransferFailures.InsufficientPaymentFunds)

  def validateAssetInputs(
    assetBoxes: List[(Address, AssetBox)],
    minting:    Boolean
  ): Either[UnsignedTransferFailure, List[(Address, AssetBox)]] =
    if (!minting)
      for {
        _ <- Either.cond(assetBoxes.nonEmpty, assetBoxes, UnsignedTransferFailures.EmptyInputs)
        _ <- Either.cond(
          assetBoxes.map(_._2.nonce).distinct.length == assetBoxes.length,
          assetBoxes,
          UnsignedTransferFailures.DuplicateInputs
        )
        _ <- Either.cond(
          assetBoxes.map(_._2.value.assetCode).distinct.length == 1,
          assetBoxes,
          UnsignedTransferFailures.DuplicateAssetCodes
        )
      } yield assetBoxes
    else assetBoxes.asRight

  def validateSameAssetCode(
    expectedAssetCode: AssetCode,
    assetBoxes:        List[(Address, AssetBox)],
    minting:           Boolean
  ): Either[UnsignedTransferFailure, AssetCode] =
    if (!minting)
      Either.cond(
        assetBoxes.head._2.value.assetCode == expectedAssetCode,
        expectedAssetCode,
        UnsignedTransferFailures.DifferentInputOutputCodes
      )
    else
      expectedAssetCode.asRight
}

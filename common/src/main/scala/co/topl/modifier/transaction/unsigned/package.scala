package co.topl.modifier.transaction

import cats.Show
import cats.implicits._
import co.topl.attestation.Address
import co.topl.modifier.box._
import co.topl.modifier.transaction.TransferTransaction.encodeFrom
import co.topl.modifier.transaction.unsigned.UnsignedTransferTransaction.getIdBytes
import co.topl.modifier.transaction.unsigned.Validation._
import co.topl.utils.Int128
import co.topl.utils.codecs.implicits._
import io.circe.Encoder
import io.circe.syntax._

import java.time.Instant

package object unsigned {

  sealed trait UnsignedTransferFailure

  object UnsignedTransferFailures {
    case object DuplicateInputs extends UnsignedTransferFailure
    case object EmptyInputs extends UnsignedTransferFailure
    case object EmptyRecipients extends UnsignedTransferFailure
    case object DuplicateRecipients extends UnsignedTransferFailure
    case object InsufficientFeeFunds extends UnsignedTransferFailure
    case object InsufficientPaymentFunds extends UnsignedTransferFailure
    case object DifferentInputOutputCodes extends UnsignedTransferFailure
    case object DuplicateAssetCodes extends UnsignedTransferFailure
  }

  type UnsignedTransferTransactionResult = Either[UnsignedTransferFailure, UnsignedTransferTransaction]

  def fromPolyTransferRequest(
    request:       TransferRequests.PolyTransferRequest,
    polyBoxInputs: List[(Address, PolyBox)]
  ): UnsignedTransferTransactionResult =
    for {
      _             <- validateNonEmptyInputs[SimpleValue, PolyBox](polyBoxInputs)
      _             <- validateUniqueInputs[SimpleValue, PolyBox](polyBoxInputs)
      _             <- validateNonEmptyRecipients(request.to)
      _             <- validateUniqueRecipients(request.to)
      amountToSpend <- validateFeeFunds(boxFunds(polyBoxInputs), request.fee)
      paymentAmount = request.to.map(_._2).sum
      changeAmount <- validatePaymentFunds(amountToSpend, paymentAmount)
      changeRecipient = request.changeAddress -> TokenValues.PolyTokens(SimpleValue(changeAmount))
      recipientValues = request.to.map(x => x._1 -> TokenValues.PolyTokens(SimpleValue(x._2)))
      nonZeroRecipients = (changeRecipient +: recipientValues).filter(x => TokenValue.getIntValue(x._2) > 0)
      fromBoxNonces = polyBoxInputs.map(box => box._1 -> box._2.nonce)
    } yield UnsignedTransferTransaction(
      request.propositionType,
      TokenTypes.Polys,
      fromBoxNonces,
      nonZeroRecipients,
      request.fee,
      Instant.now.toEpochMilli,
      request.data,
      false
    )

  def fromAssetTransferRequest(
    request:        TransferRequests.AssetTransferRequest,
    polyBoxInputs:  List[(Address, PolyBox)],
    assetBoxInputs: List[(Address, AssetBox)]
  ): UnsignedTransferTransactionResult =
    for {
      _ <- validateNonEmptyInputs[SimpleValue, PolyBox](polyBoxInputs)
      _ <- validateUniqueInputs[SimpleValue, PolyBox](polyBoxInputs)
      _ <- validateAssetInputs(assetBoxInputs, request.minting)
      _ <- validateNonEmptyRecipients(request.to)
      _ <- validateUniqueRecipients(request.to)
      assetCode = request.to.head._2.assetCode
      _          <- validateSameAssetCode(assetCode, assetBoxInputs, request.minting)
      polyChange <- validateFeeFunds(boxFunds(polyBoxInputs), request.fee)
      assetPayment = request.to.map(_._2.quantity).sum
      assetChange <-
        if (!request.minting) validatePaymentFunds(boxFunds(assetBoxInputs), assetPayment)
        else Int128(0).asRight
      assetBoxNonces = assetBoxInputs.map(box => box._1 -> box._2.nonce)
      polyBoxNonces = polyBoxInputs.map(box => box._1 -> box._2.nonce)
      changeOutput = request.changeAddress       -> TokenValues.PolyTokens(SimpleValue(polyChange))
      assetOutput = request.consolidationAddress -> TokenValues.AssetTokens(AssetValue(assetChange, assetCode))
      assetRecipients = request.to.map(x => x._1 -> TokenValues.AssetTokens(x._2))
      nonZeroOutputs = (changeOutput +: assetOutput +: assetRecipients).filter(x => TokenValue.getIntValue(x._2) > 0)
    } yield UnsignedTransferTransaction(
      request.propositionType,
      TokenTypes.Assets,
      polyBoxNonces ++ assetBoxNonces,
      nonZeroOutputs,
      request.fee,
      Instant.now.toEpochMilli,
      request.data,
      request.minting
    )

  def fromArbitTransferRequest(
    request:        TransferRequests.ArbitTransferRequest,
    polyBoxInputs:  List[(Address, PolyBox)],
    arbitBoxInputs: List[(Address, ArbitBox)]
  ): UnsignedTransferTransactionResult =
    for {
      _      <- validateNonEmptyInputs[SimpleValue, PolyBox](polyBoxInputs)
      _      <- validateUniqueInputs[SimpleValue, PolyBox](polyBoxInputs)
      _      <- validateNonEmptyInputs[SimpleValue, ArbitBox](arbitBoxInputs)
      _      <- validateUniqueInputs[SimpleValue, ArbitBox](arbitBoxInputs)
      _      <- validateNonEmptyRecipients(request.to)
      _      <- validateUniqueRecipients(request.to)
      change <- validateFeeFunds(polyBoxInputs.map(_._2.value.quantity).sum, request.fee)
      arbitsAvailable = arbitBoxInputs.map(_._2.value.quantity).sum
      arbitsToSend = request.to.map(_._2).sum
      arbitChange <- validatePaymentFunds(arbitsAvailable, arbitsToSend)
      changeOutput = request.changeAddress             -> TokenValues.PolyTokens(SimpleValue(change))
      arbitChangeOutput = request.consolidationAddress -> TokenValues.ArbitTokens(SimpleValue(arbitChange))
      arbitRecipients = request.to.map(x => x._1 -> TokenValues.ArbitTokens(SimpleValue(x._2)))
      nonZeroOutputs = (changeOutput +: arbitChangeOutput +: arbitRecipients).filter(x =>
        TokenValue.getIntValue(x._2) > 0
      )
    } yield UnsignedTransferTransaction(
      request.propositionType,
      TokenTypes.Arbits,
      (polyBoxInputs ++ arbitBoxInputs).map(x => x._1 -> x._2.nonce),
      nonZeroOutputs,
      request.fee,
      Instant.now.toEpochMilli,
      request.data,
      false
    )

  trait Implicits {

    implicit val showUnsignedTransferFailure: Show[UnsignedTransferFailure] =
      failure => {
        val generalMessage = "Failed to create an unsigned transfer transaction: "

        val message = failure match {
          case UnsignedTransferFailures.DuplicateInputs =>
            "duplicate input boxes were provided"
          case UnsignedTransferFailures.EmptyInputs =>
            "no input boxes were provided"
          case UnsignedTransferFailures.EmptyRecipients =>
            "no recipients were provided"
          case UnsignedTransferFailures.DuplicateRecipients =>
            "duplicate recipients were procided"
          case UnsignedTransferFailures.InsufficientFeeFunds =>
            "insufficient funds were provided to pay the fee"
          case UnsignedTransferFailures.InsufficientPaymentFunds =>
            "insufficient funds were provided to make payments"
          case UnsignedTransferFailures.DifferentInputOutputCodes =>
            "different input/output asset codes were provided"
          case UnsignedTransferFailures.DuplicateAssetCodes =>
            "duplicate asset codes were provided"
        }

        generalMessage + message
      }

  }

  object implicits extends Implicits
}

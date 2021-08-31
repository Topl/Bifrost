package co.topl.modifier.transaction

import cats.implicits._
import co.topl.attestation.Address
import co.topl.modifier.box.{PolyBox, SimpleValue, TokenValueHolder}
import co.topl.utils.Int128
import co.topl.utils.StringDataTypes.Latin1Data

trait Transferable[T] {
  def id(t: T): Long
  def quantity(t: T): Int128
}

case class TransferParams[Fee: ValueBox, Token: ValueBox](feeBoxes: )

object TransferParams {



  type Builder[F: ValueBox, T: ValueBox] = (List[F], List[T], List[(Address, T)], Int128)
}

abstract class ValidatedTransferParams[T, Failure, Transfer] {
  def validate(t: T): Either[Failure, T]

  def generateTransfer(t: T): Transfer
}

abstract class ValidatedTransfer[Params, Failure, Transfer] {
  def validateParams(params: Params): Either[Failure, Params]

  def generateTransfer(
    timestamp: Long,
    fee:       Int128,
    changeBox: (Address, SimpleValue),
    data:      Option[Latin1Data],
    params:    Params
  ): Transfer
}

object TransactionBuilder {

  type TransactionResult[Failure, Transfer] = Either[Either[TransactionMetaFailure, Failure], Transfer]

  sealed trait TransactionMetaFailure
  case object EmptyFeeBoxes extends TransactionMetaFailure
  case object DuplicateFeeBoxes extends TransactionMetaFailure
  case object InsufficientFeeFunds extends TransactionMetaFailure

  private def validateFeeBoxes(
    boxes: IndexedSeq[PolyBox],
    fee:   Int128
  ): Either[TransactionMetaFailure, Int128] =
    for {
      _ <- Either.cond(boxes.nonEmpty, boxes, EmptyFeeBoxes)
      _ <- Either.cond(boxes.distinctBy(_.nonce).length == boxes.length, boxes, DuplicateFeeBoxes)
      changeAmount = boxes.map(_.value.quantity).sum
      _ <- Either.cond(changeAmount >= 0, changeAmount, InsufficientFeeFunds)
    } yield changeAmount

  def buildTx[Params, Failure, Transfer](params: Params, feeBoxes: IndexedSeq[(Address, SimpleValue)], fee: Int128)(
    implicit validatedTransferParams:            ValidatedTransferParams[Params, Failure, Transfer]
  ): TransactionResult[Failure, Transfer] =
    for {
      changeAmount <- validateFeeBoxes(feeBoxes.map(_._2), fee)
      _            <- validatedTransfer.validateParams(params).leftMap(ParamsValidationFailure)
      transfer = validatedTransfer.generateTransfer(
        timestamp,
        fee,
        (changeAddress, SimpleValue(changeAmount)),
        data,
        params
      )
    } yield transfer
}

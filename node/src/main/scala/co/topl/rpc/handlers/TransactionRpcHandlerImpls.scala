package co.topl.rpc.handlers

import cats.implicits._
import co.topl.akkahttprpc.{CustomError, RpcError, ThrowableData}
import co.topl.attestation._
import co.topl.modifier.box.{AssetValue, SimpleValue}
import co.topl.modifier.transaction
import co.topl.modifier.transaction.ArbitTransfer.Validation.InvalidArbitTransfer
import co.topl.modifier.transaction.AssetTransfer.Validation.InvalidAssetTransfer
import co.topl.modifier.transaction.PolyTransfer.Validation.InvalidPolyTransfer
import co.topl.modifier.transaction.builder.BoxPickingStrategy
import co.topl.modifier.transaction.validation.implicits._
import co.topl.modifier.transaction.{ArbitTransfer, AssetTransfer, PolyTransfer, Transaction}
import co.topl.nodeView.state.State
import co.topl.nodeView.{BroadcastTxFailureException, GetStateFailureException, NodeViewHolderInterface}
import co.topl.rpc.{ToplRpc, ToplRpcErrors}
import co.topl.utils.IdiomaticScalaTransition.implicits.toEitherOps
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.StringDataTypes.implicits._
import co.topl.utils.codecs.implicits._
import io.circe.Encoder
import co.topl.modifier.transaction.builder.implicits._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class TransactionRpcHandlerImpls(
  nodeViewHolderInterface: NodeViewHolderInterface
)(implicit
  ec:               ExecutionContext,
  throwableEncoder: Encoder[ThrowableData],
  networkPrefix:    NetworkPrefix
) extends ToplRpcHandlers.Transaction {

  override val rawAssetTransfer: ToplRpc.Transaction.RawAssetTransfer.rpc.ServerHandler =
    params =>
      for {
        state           <- currentState()
        senderAddresses <- checkAddresses(params.sender.toList, state).toEitherT[Future]
        transferTry = tryCreateAssetTransfer(params, state, senderAddresses)
        transfer <- Either
          .fromTry(transferTry)
          .leftMap[RpcError](ToplRpcErrors.transactionValidationException(_))
          .toEitherT[Future]
        messageToSign = transfer.messageToSign.encodeAsBase58
      } yield ToplRpc.Transaction.RawAssetTransfer.Response(transfer, messageToSign.show)

  override val rawArbitTransfer: ToplRpc.Transaction.RawArbitTransfer.rpc.ServerHandler =
    params =>
      for {
        state           <- currentState()
        senderAddresses <- checkAddresses(params.sender.toList, state).toEitherT[Future]
        transferTry = tryCreateArbitTransfer(params, state, senderAddresses)
        transfer <- Either
          .fromTry(transferTry)
          .leftMap[RpcError](ToplRpcErrors.transactionValidationException(_))
          .toEitherT[Future]
        messageToSign = transfer.messageToSign.encodeAsBase58
      } yield ToplRpc.Transaction.RawArbitTransfer.Response(transfer, messageToSign.show)

  override val rawPolyTransfer: ToplRpc.Transaction.RawPolyTransfer.rpc.ServerHandler =
    params =>
      for {
        state           <- currentState()
        senderAddresses <- checkAddresses(params.sender.toList, state).toEitherT[Future]
        transferTry = tryCreatePolyTransfer(params, state, senderAddresses)
        transfer <- Either
          .fromTry(transferTry)
          .leftMap[RpcError](ToplRpcErrors.transactionValidationException(_))
          .toEitherT[Future]
        messageToSign = transfer.messageToSign.encodeAsBase58
      } yield ToplRpc.Transaction.RawPolyTransfer.Response(transfer, messageToSign.show)

  override val broadcastTx: ToplRpc.Transaction.BroadcastTx.rpc.ServerHandler =
    params =>
      for {
        transaction <- params.tx.syntacticValidation.toEither
          .leftMap(ToplRpcErrors.syntacticValidationFailure)
          .toEitherT[Future]
        _ <- processTransaction(transaction)
      } yield transaction

  private def tryCreateAssetTransfer(
    params:          ToplRpc.Transaction.RawAssetTransfer.Params,
    state:           State,
    senderAddresses: List[Address]
  ): Try[AssetTransfer[Proposition]] = Try {
    val createRaw = params.propositionType match {
      case PublicKeyPropositionCurve25519.`typeString` =>
        transaction.builder
          .buildTransfer[AssetValue, InvalidAssetTransfer, AssetTransfer[
            PublicKeyPropositionCurve25519
          ], BoxPickingStrategy.All] _
      case ThresholdPropositionCurve25519.`typeString` =>
        transaction.builder
          .buildTransfer[AssetValue, InvalidAssetTransfer, AssetTransfer[
            ThresholdPropositionCurve25519
          ], BoxPickingStrategy.All] _
      case PublicKeyPropositionEd25519.`typeString` =>
        transaction.builder
          .buildTransfer[AssetValue, InvalidAssetTransfer, AssetTransfer[
            PublicKeyPropositionEd25519
          ], BoxPickingStrategy.All] _
    }

    createRaw(
      senderAddresses.toIndexedSeq,
      params.recipients.toList.toIndexedSeq,
      state,
      params.changeAddress,
      params.fee,
      Some(params.consolidationAddress),
      params.data,
      params.minting,
      BoxPickingStrategy.All
    ).getOrThrow()
  }.collect { case p: AssetTransfer[Proposition @unchecked] => p }

  private def tryCreateArbitTransfer(
    params:          ToplRpc.Transaction.RawArbitTransfer.Params,
    state:           State,
    senderAddresses: List[Address]
  ): Try[ArbitTransfer[Proposition]] = Try {
    val createRaw = params.propositionType match {
      case PublicKeyPropositionCurve25519.`typeString` =>
        transaction.builder
          .buildTransfer[SimpleValue, InvalidArbitTransfer, ArbitTransfer[
            PublicKeyPropositionCurve25519
          ], BoxPickingStrategy.All] _
      case ThresholdPropositionCurve25519.`typeString` =>
        transaction.builder
          .buildTransfer[SimpleValue, InvalidArbitTransfer, ArbitTransfer[
            ThresholdPropositionCurve25519
          ], BoxPickingStrategy.All] _
      case PublicKeyPropositionEd25519.`typeString` =>
        transaction.builder
          .buildTransfer[SimpleValue, InvalidArbitTransfer, ArbitTransfer[
            PublicKeyPropositionEd25519
          ], BoxPickingStrategy.All] _
    }

    createRaw(
      senderAddresses.toIndexedSeq,
      params.recipients.toList.toIndexedSeq.map(tup => tup._1 -> SimpleValue(tup._2)),
      state,
      params.changeAddress,
      params.fee,
      Some(params.consolidationAddress),
      params.data,
      false,
      BoxPickingStrategy.All
    ).getOrThrow()
  }.collect { case p: ArbitTransfer[Proposition @unchecked] => p }

  private def tryCreatePolyTransfer(
    params:          ToplRpc.Transaction.RawPolyTransfer.Params,
    state:           State,
    senderAddresses: List[Address]
  ): Try[PolyTransfer[Proposition]] = Try {
    val createRaw = params.propositionType match {
      case PublicKeyPropositionCurve25519.`typeString` =>
        transaction.builder
          .buildTransfer[SimpleValue, InvalidPolyTransfer, PolyTransfer[
            PublicKeyPropositionCurve25519
          ], BoxPickingStrategy.All] _
      case ThresholdPropositionCurve25519.`typeString` =>
        transaction.builder
          .buildTransfer[SimpleValue, InvalidPolyTransfer, PolyTransfer[
            ThresholdPropositionCurve25519
          ], BoxPickingStrategy.All] _
      case PublicKeyPropositionEd25519.`typeString` =>
        transaction.builder
          .buildTransfer[SimpleValue, InvalidPolyTransfer, PolyTransfer[
            PublicKeyPropositionEd25519
          ], BoxPickingStrategy.All] _
    }

    createRaw(
      senderAddresses.toIndexedSeq,
      params.recipients.toList.toIndexedSeq.map(tup => tup._1 -> SimpleValue(tup._2)),
      state,
      params.changeAddress,
      params.fee,
      None,
      params.data,
      false,
      BoxPickingStrategy.All
    ).getOrThrow()
  }.collect { case p: PolyTransfer[Proposition @unchecked] => p }

  private def processTransaction(tx: Transaction.TX) =
    nodeViewHolderInterface.broadcastTransaction(tx).leftMap { case BroadcastTxFailureException(throwable) =>
      CustomError.fromThrowable(throwable): RpcError
    }

  private def currentState() =
    nodeViewHolderInterface.getState().leftMap { case GetStateFailureException(throwable) =>
      CustomError.fromThrowable(throwable): RpcError
    }
}

package co.topl.rpc.handlers

import akka.actor.typed.ActorSystem
import cats.implicits._
import co.topl.akkahttprpc.{CustomError, RpcError, ThrowableData}
import co.topl.attestation._
import co.topl.modifier.box.{AssetValue, ProgramId, SimpleValue}
import co.topl.modifier.transaction
import co.topl.modifier.transaction.ArbitTransfer.Validation.InvalidArbitTransfer
import co.topl.modifier.transaction.AssetTransfer.Validation.InvalidAssetTransfer
import co.topl.modifier.transaction.PolyTransfer.Validation.InvalidPolyTransfer
import co.topl.modifier.transaction.builder.BoxPickingStrategy
import co.topl.modifier.transaction.builder.implicits._
import co.topl.modifier.transaction.validation.implicits._
import co.topl.modifier.transaction.{ArbitTransfer, AssetTransfer, PolyTransfer, Transaction}
import co.topl.nodeView.state.StateReader
import co.topl.nodeView.{NodeViewHolderInterface, ReadableNodeView}
import co.topl.rpc.{ToplRpc, ToplRpcErrors}
import co.topl.utils.IdiomaticScalaTransition.implicits.toEitherOps
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.StringDataTypes.implicits._
import co.topl.utils.codecs.implicits._
import io.circe.Encoder

import scala.concurrent.Future
import scala.util.Try

class TransactionRpcHandlerImpls(
  nodeViewHolderInterface: NodeViewHolderInterface
)(implicit
  system:           ActorSystem[_],
  throwableEncoder: Encoder[ThrowableData],
  networkPrefix:    NetworkPrefix
) extends ToplRpcHandlers.Transaction {

  import system.executionContext

  override val rawAssetTransfer: ToplRpc.Transaction.RawAssetTransfer.rpc.ServerHandler =
    params =>
      for {
        transferTry <- withNodeView(view =>
          checkAddresses(params.sender.toList, view.state)
            .map(tryCreateAssetTransfer(params, view.state, _))
        ).subflatMap(identity)
        transfer <- Either
          .fromTry(transferTry)
          .leftMap[RpcError](ToplRpcErrors.transactionValidationException(_))
          .toEitherT[Future]
        messageToSign = transfer.messageToSign.encodeAsBase58
      } yield ToplRpc.Transaction.RawAssetTransfer.Response(transfer, messageToSign.show)

  override val rawArbitTransfer: ToplRpc.Transaction.RawArbitTransfer.rpc.ServerHandler =
    params =>
      for {
        transferTry <- withNodeView(view =>
          checkAddresses(params.sender.toList, view.state)
            .map(tryCreateArbitTransfer(params, view.state, _))
        ).subflatMap(identity)
        transfer <- Either
          .fromTry(transferTry)
          .leftMap[RpcError](ToplRpcErrors.transactionValidationException(_))
          .toEitherT[Future]
        messageToSign = transfer.messageToSign.encodeAsBase58
      } yield ToplRpc.Transaction.RawArbitTransfer.Response(transfer, messageToSign.show)

  override val rawPolyTransfer: ToplRpc.Transaction.RawPolyTransfer.rpc.ServerHandler =
    params =>
      for {
        transferTry <- withNodeView(view =>
          checkAddresses(params.sender.toList, view.state)
            .map(tryCreatePolyTransfer(params, view.state, _))
        ).subflatMap(identity)
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
    state:           StateReader[ProgramId, Address],
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
      BoxPickingStrategy.All,
      Some(params.consolidationAddress),
      params.data,
      params.minting
    ).getOrThrow()
  }.collect { case p: AssetTransfer[Proposition @unchecked] => p }

  private def tryCreateArbitTransfer(
    params:          ToplRpc.Transaction.RawArbitTransfer.Params,
    state:           StateReader[ProgramId, Address],
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
      BoxPickingStrategy.All,
      Some(params.consolidationAddress),
      params.data,
      false
    ).getOrThrow()
  }.collect { case p: ArbitTransfer[Proposition @unchecked] => p }

  private def tryCreatePolyTransfer(
    params:          ToplRpc.Transaction.RawPolyTransfer.Params,
    state:           StateReader[ProgramId, Address],
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
      BoxPickingStrategy.All,
      None,
      params.data,
      false
    ).getOrThrow()
  }.collect { case p: PolyTransfer[Proposition @unchecked] => p }

  private def processTransaction(tx: Transaction.TX) =
    nodeViewHolderInterface.applyTransactions(tx).leftMap { case NodeViewHolderInterface.ApplyFailure(throwable) =>
      CustomError.fromThrowable(throwable): RpcError
    }

  private def withNodeView[T](f: ReadableNodeView => T) =
    nodeViewHolderInterface
      .withNodeView(f)
      .leftMap { case NodeViewHolderInterface.ReadFailure(throwable) =>
        CustomError.fromThrowable(throwable): RpcError
      }
}

package co.topl.rpc.handlers

import akka.actor.typed.ActorSystem
import cats.implicits._
import co.topl.akkahttprpc.{CustomError, RpcError, ThrowableData}
import co.topl.attestation._
import co.topl.codecs._
import co.topl.modifier.ProgramId
import co.topl.modifier.transaction.builder.{BuildTransferFailure, TransferBuilder, TransferRequests}
import co.topl.modifier.transaction.validation.implicits._
import co.topl.modifier.transaction.{ArbitTransfer, AssetTransfer, PolyTransfer, Transaction}
import co.topl.nodeView.state.StateReader
import co.topl.nodeView.{NodeViewHolderInterface, ReadableNodeView}
import co.topl.rpc.{ToplRpc, ToplRpcErrors}
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.implicits._
import io.circe.Encoder
import co.topl.modifier.ops.implicits._
import io.circe.syntax._

import scala.concurrent.Future

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
        unsignedTx <- withNodeView(view => createAssetTransfer(params, view.state))
        transfer <- unsignedTx
          .leftMap(failure => new Error(failure.show))
          .leftMap[RpcError](ToplRpcErrors.transactionValidationException(_))
          .toEitherT[Future]
        messageToSign = transfer.messageToSign.encodeAsBase58
      } yield ToplRpc.Transaction.RawAssetTransfer.Response(transfer, messageToSign.show)

  override val rawArbitTransfer: ToplRpc.Transaction.RawArbitTransfer.rpc.ServerHandler =
    params =>
      for {
        unsignedTx <- withNodeView(view => createArbitTransfer(params, view.state))
        transfer <- unsignedTx
          .leftMap(failure => new Error(failure.show))
          .leftMap[RpcError](ToplRpcErrors.transactionValidationException(_))
          .toEitherT[Future]
        messageToSign = transfer.messageToSign.encodeAsBase58
      } yield ToplRpc.Transaction.RawArbitTransfer.Response(transfer, messageToSign.show)

  override val rawPolyTransfer: ToplRpc.Transaction.RawPolyTransfer.rpc.ServerHandler =
    params =>
      for {
        unsignedTx <- withNodeView(view => tryCreatePolyTransfer(params, view.state))
        transfer <- unsignedTx
          .leftMap(failure => new Error(failure.show))
          .leftMap[RpcError](ToplRpcErrors.transactionValidationException(_))
          .toEitherT[Future]
        messageToSign = {
          println(
            s"\n >>>>>>>>>>>>>>>>> Newly created transaction from RPC: \n${transfer.asJson} from params: ${params}"
          )
          transfer.messageToSign.encodeAsBase58
        }
      } yield ToplRpc.Transaction.RawPolyTransfer.Response(transfer, messageToSign.show)

  override val broadcastTx: ToplRpc.Transaction.BroadcastTx.rpc.ServerHandler =
    params =>
      for {
        transaction <- params.tx.syntacticValidation.toEither
          .leftMap(ToplRpcErrors.syntacticValidationFailure)
          .toEitherT[Future]
        _ <- processTransaction(transaction)
      } yield transaction

  override val encodeTransfer: ToplRpc.Transaction.EncodeTransfer.rpc.ServerHandler =
    params =>
      for {
        rawTransaction <- params.unprovenTransaction.rawSyntacticValidation.toEither
          .leftMap[RpcError](ToplRpcErrors.syntacticValidationFailure)
          .toEitherT[Future]
        messageToSign = rawTransaction.messageToSign.encodeAsBase58
      } yield ToplRpc.Transaction.EncodeTransfer.Response(messageToSign.show)

  override val unprovenPolyTransfer: ToplRpc.Transaction.UnprovenPolyTransfer.rpc.ServerHandler =
    params =>
      for {
        unprovenTransferResult <-
          withNodeView(view =>
            TransferBuilder
              .buildUnprovenTransfer(
                view.state,
                TransferRequests.UnprovenTransferRequest(
                  params.senders.toList,
                  params.recipients.toList,
                  params.changeAddress,
                  params.changeAddress,
                  params.fee,
                  params.data,
                  minting = false
                ),
                params.boxSelectionAlgorithm
              )
          )
        transfer <-
          unprovenTransferResult
            .leftMap(failure => new Error(failure.show))
            .leftMap[RpcError](ToplRpcErrors.transactionValidationException(_))
            .toEitherT[Future]
      } yield ToplRpc.Transaction.UnprovenPolyTransfer.Response(transfer)

  override val unprovenArbitTransfer: ToplRpc.Transaction.UnprovenArbitTransfer.rpc.ServerHandler =
    params =>
      for {
        unprovenTransferResult <-
          withNodeView(view =>
            TransferBuilder
              .buildUnprovenTransfer(
                view.state,
                TransferRequests.UnprovenTransferRequest(
                  params.senders.toList,
                  params.recipients.toList,
                  params.changeAddress,
                  params.changeAddress,
                  params.fee,
                  params.data,
                  minting = false
                ),
                params.boxSelectionAlgorithm
              )
          )
        transfer <-
          unprovenTransferResult
            .leftMap(failure => new Error(failure.show))
            .leftMap[RpcError](ToplRpcErrors.transactionValidationException(_))
            .toEitherT[Future]
      } yield ToplRpc.Transaction.UnprovenArbitTransfer.Response(transfer)

  override val unprovenAssetTransfer: ToplRpc.Transaction.UnprovenAssetTransfer.rpc.ServerHandler =
    params =>
      for {
        unprovenTransferResult <-
          withNodeView(view =>
            TransferBuilder
              .buildUnprovenTransfer(
                view.state,
                TransferRequests.UnprovenTransferRequest(
                  params.senders.toList,
                  params.recipients.toList,
                  params.feeChangeAddress,
                  params.assetChangeAddress,
                  params.fee,
                  params.data,
                  params.minting
                ),
                params.boxSelectionAlgorithm
              )
          )
        transfer <-
          unprovenTransferResult
            .leftMap(failure => new Error(failure.show))
            .leftMap[RpcError](ToplRpcErrors.transactionValidationException(_))
            .toEitherT[Future]
      } yield ToplRpc.Transaction.UnprovenAssetTransfer.Response(transfer)

  override val broadcastTetraTransfer: ToplRpc.Transaction.BroadcastTetraTransfer.rpc.ServerHandler =
    params =>
      for {
        dionTx <- params.transfer.toDionTx
          .leftMap(failure => ToplRpcErrors.genericFailure(failure.toString))
          .toEitherT[Future]
        tx <-
          dionTx.syntacticValidation.toEither
            .leftMap(ToplRpcErrors.syntacticValidationFailure)
            .toEitherT[Future]
        _ <- processTransaction(tx)
      } yield tx

  private def createAssetTransfer(
    params: ToplRpc.Transaction.RawAssetTransfer.Params,
    state:  StateReader[ProgramId, Address]
  ): Either[BuildTransferFailure, AssetTransfer[Proposition]] = {

    val transferRequest =
      TransferRequests.AssetTransferRequest(
        params.sender.toList,
        params.recipients.toList,
        params.changeAddress,
        params.consolidationAddress,
        params.fee,
        params.data,
        params.minting
      )

    (params.propositionType match {
      case PublicKeyPropositionCurve25519.`typeString` =>
        TransferBuilder
          .buildUnsignedAssetTransfer[PublicKeyPropositionCurve25519](
            state,
            transferRequest,
            params.boxSelectionAlgorithm
          )
      case ThresholdPropositionCurve25519.`typeString` =>
        TransferBuilder.buildUnsignedAssetTransfer[ThresholdPropositionCurve25519](
          state,
          transferRequest,
          params.boxSelectionAlgorithm
        )
      case PublicKeyPropositionEd25519.`typeString` =>
        TransferBuilder.buildUnsignedAssetTransfer[PublicKeyPropositionEd25519](
          state,
          transferRequest,
          params.boxSelectionAlgorithm
        )
      // need to cast to type AssetTransfer[Proposition] because the P type param in AssetTransfer
      // is invariant and not co-variant
    }).map(_.asInstanceOf[AssetTransfer[Proposition]])
  }

  private def createArbitTransfer(
    params: ToplRpc.Transaction.RawArbitTransfer.Params,
    state:  StateReader[ProgramId, Address]
  ): Either[BuildTransferFailure, ArbitTransfer[Proposition]] = {

    val transferRequest =
      TransferRequests.ArbitTransferRequest(
        params.sender.toList,
        params.recipients.toList,
        params.changeAddress,
        params.consolidationAddress,
        params.fee,
        params.data
      )

    (params.propositionType match {
      case PublicKeyPropositionCurve25519.`typeString` =>
        TransferBuilder.buildUnsignedArbitTransfer[PublicKeyPropositionCurve25519](
          state,
          transferRequest,
          params.boxSelectionAlgorithm
        )
      case ThresholdPropositionCurve25519.`typeString` =>
        TransferBuilder.buildUnsignedArbitTransfer[ThresholdPropositionCurve25519](
          state,
          transferRequest,
          params.boxSelectionAlgorithm
        )
      case PublicKeyPropositionEd25519.`typeString` =>
        TransferBuilder.buildUnsignedArbitTransfer[PublicKeyPropositionEd25519](
          state,
          transferRequest,
          params.boxSelectionAlgorithm
        )
      // need to cast to type ArbitTransfer[Proposition] because the P type param in ArbitTransfer
      // is invariant and not co-variant
    }).map(_.asInstanceOf[ArbitTransfer[Proposition]])
  }

  private def tryCreatePolyTransfer(
    params: ToplRpc.Transaction.RawPolyTransfer.Params,
    state:  StateReader[ProgramId, Address]
  ): Either[BuildTransferFailure, PolyTransfer[Proposition]] = {

    val transferRequest =
      TransferRequests.PolyTransferRequest(
        params.sender.toList,
        params.recipients.toList,
        params.changeAddress,
        params.fee,
        params.data
      )

    (params.propositionType match {
      case PublicKeyPropositionCurve25519.`typeString` =>
        TransferBuilder.buildUnsignedPolyTransfer[PublicKeyPropositionCurve25519](
          state,
          transferRequest,
          params.boxSelectionAlgorithm
        )
      case ThresholdPropositionCurve25519.`typeString` =>
        TransferBuilder.buildUnsignedPolyTransfer[ThresholdPropositionCurve25519](
          state,
          transferRequest,
          params.boxSelectionAlgorithm
        )
      case PublicKeyPropositionEd25519.`typeString` =>
        TransferBuilder.buildUnsignedPolyTransfer[PublicKeyPropositionEd25519](
          state,
          transferRequest,
          params.boxSelectionAlgorithm
        )
      // need to cast to type PolyTransfer[Proposition] because the P type param in PolyTransfer
      // is invariant and not co-variant
    }).map(_.asInstanceOf[PolyTransfer[Proposition]])
  }

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

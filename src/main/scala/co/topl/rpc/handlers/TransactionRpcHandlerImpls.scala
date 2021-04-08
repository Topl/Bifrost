package co.topl.rpc.handlers

import cats.data.EitherT
import cats.implicits._
import co.topl.akkahttprpc.{RpcError, ThrowableData}
import co.topl.attestation.{Address, Proposition, PublicKeyPropositionCurve25519, ThresholdPropositionCurve25519}
import co.topl.modifier.box.SimpleValue
import co.topl.modifier.transaction.{ArbitTransfer, AssetTransfer, PolyTransfer}
import co.topl.nodeView.state.State
import co.topl.rpc.{ToplRpc, ToplRpcErrors}
import co.topl.utils.NetworkType.NetworkPrefix
import io.circe.Encoder
import scorex.util.encode.Base58

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class TransactionRpcHandlerImpls(getState: GetState, processTransaction: ProcessTransaction)(implicit
  ec:                                      ExecutionContext,
  throwableEncoder:                        Encoder[ThrowableData],
  networkPrefix:                           NetworkPrefix
) extends ToplRpcHandlers.Transaction {

  override val rawAssetTransfer: ToplRpc.Transaction.RawAssetTransfer.rpc.ServerHandler =
    params =>
      for {
        state           <- getState()
        senderAddresses <- EitherT.fromEither[Future](checkAddresses(params.sender.toList, state))
        transferTry = tryCreateAssetTransfer(params, state, senderAddresses)
        transfer <- EitherT.fromEither[Future](
          Either.fromTry(transferTry).leftMap[RpcError[_]](ToplRpcErrors.transactionValidationException(_))
        )
        messageToSign = Base58.encode(transfer.messageToSign)
      } yield ToplRpc.Transaction.RawAssetTransfer.Response(transfer, messageToSign)

  override val rawArbitTransfer: ToplRpc.Transaction.RawArbitTransfer.rpc.ServerHandler =
    params =>
      for {
        state           <- getState()
        senderAddresses <- EitherT.fromEither[Future](checkAddresses(params.sender.toList, state))
        transferTry = tryCreateArbitTransfer(params, state, senderAddresses)
        transfer <- EitherT.fromEither[Future](
          Either.fromTry(transferTry).leftMap[RpcError[_]](ToplRpcErrors.transactionValidationException(_))
        )
        messageToSign = Base58.encode(transfer.messageToSign)
      } yield ToplRpc.Transaction.RawArbitTransfer.Response(transfer, messageToSign)

  override val rawPolyTransfer: ToplRpc.Transaction.RawPolyTransfer.rpc.ServerHandler =
    params =>
      for {
        state           <- getState()
        senderAddresses <- EitherT.fromEither[Future](checkAddresses(params.sender.toList, state))
        transferTry = tryCreatePolyTransfer(params, state, senderAddresses)
        transfer <- EitherT.fromEither[Future](
          Either.fromTry(transferTry).leftMap[RpcError[_]](ToplRpcErrors.transactionValidationException(_))
        )
        messageToSign = Base58.encode(transfer.messageToSign)
      } yield ToplRpc.Transaction.RawPolyTransfer.Response(transfer, messageToSign)

  override def broadcastTx: ToplRpc.Transaction.BroadcastTx.rpc.ServerHandler =
    params =>
      for {
        transaction <- EitherT.fromEither[Future](
          params.tx.syntacticValidate.toEither.leftMap(ToplRpcErrors.syntacticValidationFailure)
        )
        _ <- processTransaction(transaction)
      } yield transaction

  private def tryCreateAssetTransfer(
    params:          ToplRpc.Transaction.RawAssetTransfer.Params,
    state:           State,
    senderAddresses: List[Address]
  ): Try[AssetTransfer[Proposition]] = {
    val createRaw = params.propositionType match {
      case PublicKeyPropositionCurve25519.`typeString` => AssetTransfer.createRaw[PublicKeyPropositionCurve25519] _
      case ThresholdPropositionCurve25519.`typeString` => AssetTransfer.createRaw[ThresholdPropositionCurve25519] _
    }

    createRaw(
      state,
      params.recipients.toNonEmptyVector.toVector,
      senderAddresses.to[IndexedSeq],
      params.changeAddress,
      params.consolidationAddress,
      params.fee,
      params.data,
      params.minting
    )
      .collect { case p: AssetTransfer[Proposition @unchecked] =>
        p
      }
  }

  private def tryCreateArbitTransfer(
    params:          ToplRpc.Transaction.RawArbitTransfer.Params,
    state:           State,
    senderAddresses: List[Address]
  ): Try[ArbitTransfer[Proposition]] = {
    val createRaw = params.propositionType match {
      case PublicKeyPropositionCurve25519.`typeString` => ArbitTransfer.createRaw[PublicKeyPropositionCurve25519] _
      case ThresholdPropositionCurve25519.`typeString` => ArbitTransfer.createRaw[ThresholdPropositionCurve25519] _
    }

    createRaw(
      state,
      params.recipients.toNonEmptyVector.toVector,
      senderAddresses.to[IndexedSeq],
      params.changeAddress,
      params.consolidationAddress,
      params.fee,
      params.data
    )
      .collect { case p: ArbitTransfer[Proposition @unchecked] =>
        p
      }
  }

  private def tryCreatePolyTransfer(
    params:          ToplRpc.Transaction.RawPolyTransfer.Params,
    state:           State,
    senderAddresses: List[Address]
  ): Try[PolyTransfer[Proposition]] = {
    val f =
      params.propositionType match {
        case PublicKeyPropositionCurve25519.`typeString` =>
          PolyTransfer.createRaw[PublicKeyPropositionCurve25519] _
        case ThresholdPropositionCurve25519.`typeString` =>
          PolyTransfer.createRaw[ThresholdPropositionCurve25519] _
      }

    f(
      state,
      params.recipients.map { case (address, v) => address -> SimpleValue(v) }.toNonEmptyVector.toVector,
      senderAddresses.to[IndexedSeq],
      params.changeAddress,
      None,
      params.fee,
      params.data
    )
  }.collect { case p: PolyTransfer[Proposition @unchecked] => p }
}

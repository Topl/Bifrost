package co.topl.rpc.handlers

import cats.data.EitherT
import cats.implicits._
import co.topl.akkahttprpc.{InvalidParametersError, RpcError, ThrowableData}
import co.topl.attestation.Address
import co.topl.modifier.box._
import co.topl.nodeView.state.State
import co.topl.rpc.ToplRpc
import co.topl.settings.AppContext
import co.topl.utils.Int128
import co.topl.utils.NetworkType.NetworkPrefix
import io.circe.Encoder

import scala.concurrent.{ExecutionContext, Future}
import scala.language.existentials

class NodeViewRpcHandlerImpls(
  appContext: AppContext,
  getHistory: GetHistory,
  getState:   GetState,
  getPool:    GetMempool
)(implicit
  ec:               ExecutionContext,
  throwableEncoder: Encoder[ThrowableData],
  networkPrefix:    NetworkPrefix
) extends ToplRpcHandlers.NodeView {

  override val head: ToplRpc.NodeView.Head.rpc.ServerHandler =
    _ =>
      for {
        history <- getHistory()
      } yield ToplRpc.NodeView.Head.Response(
        history.height,
        history.score,
        history.bestBlockId,
        history.bestBlock
      )

  override val balances: ToplRpc.NodeView.Balances.rpc.ServerHandler =
    params =>
      for {
        state     <- getState()
        addresses <- checkAddresses(params.addresses, state).toEitherT[Future]
      } yield balancesResponse(state, addresses)

  override val transactionById: ToplRpc.NodeView.TransactionById.rpc.ServerHandler =
    params =>
      for {
        history <- getHistory()
        tResult <- history
          .transactionById(params.transactionId)
          .toRight[RpcError](
            InvalidParametersError.adhoc("Unable to find confirmed transaction", "modifierId")
          )
          .toEitherT[Future]
        (tx, blockId, blockNumber) = tResult
      } yield ToplRpc.NodeView.TransactionById.Response(tx, blockNumber, blockId)

  override val blockById: ToplRpc.NodeView.BlockById.rpc.ServerHandler =
    params =>
      for {
        history <- getHistory()
        block <- history
          .modifierById(params.blockId)
          .toRight[RpcError](
            InvalidParametersError.adhoc("The requested block could not be found", "blockId")
          )
          .toEitherT[Future]
      } yield block

  override val blockByHeight: ToplRpc.NodeView.BlockByHeight.rpc.ServerHandler =
    params =>
      for {
        history <- getHistory()
        block <- history
          .modifierByHeight(params.height)
          .toRight[RpcError](
            InvalidParametersError.adhoc("The requested block could not be found", "height")
          )
          .toEitherT[Future]
      } yield block

  override val mempool: ToplRpc.NodeView.Mempool.rpc.ServerHandler =
    _ =>
      for {
        pool <- getPool()
        transactions = pool.take(100)(-_.dateAdded).map(_.tx).toList
      } yield transactions

  override val transactionFromMempool: ToplRpc.NodeView.TransactionFromMempool.rpc.ServerHandler =
    params =>
      for {
        pool <- getPool()
        tx <- pool
          .modifierById(params.transactionId)
          .toRight[RpcError](InvalidParametersError.adhoc("Unable to retrieve transaction", "transactionId"))
          .toEitherT[Future]
      } yield tx

  override val info: ToplRpc.NodeView.Info.rpc.ServerHandler =
    _ =>
      EitherT.pure(
        ToplRpc.NodeView.Info.Response(
          appContext.networkType.toString,
          appContext.externalNodeAddress.fold("N/A")(_.toString),
          appContext.settings.application.version.toString
        )
      )

  private def balancesResponse(state: State, addresses: List[Address]): ToplRpc.NodeView.Balances.Response = {
    val boxes =
      addresses.map { k =>
        val orderedBoxes = state.getTokenBoxes(k) match {
          case Some(boxes) => boxes.groupBy[String](Box.identifier(_).typeString).map { case (k, v) => k -> v.toList }
          case _           => Map[String, List[TokenBox[TokenValueHolder]]]()
        }
        k -> orderedBoxes
      }.toMap

    val balances =
      boxes.map { case (addr, assets) =>
        addr -> assets.map { case (boxType, boxes) =>
          (boxType, boxes.map(_.value.quantity).sum)
        }
      }

    boxes.map { case (addr, boxes) =>
      addr -> ToplRpc.NodeView.Balances.Entry(
        ToplRpc.NodeView.Balances.EntryBalances(
          balances(addr).getOrElse(PolyBox.typeString, Int128(0)),
          balances(addr).getOrElse(ArbitBox.typeString, Int128(0))
        ),
        boxes
      )
    }
  }
}

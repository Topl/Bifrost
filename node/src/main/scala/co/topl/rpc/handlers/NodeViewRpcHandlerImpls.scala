package co.topl.rpc.handlers

import cats.data.EitherT
import cats.implicits._
import co.topl.akkahttprpc.{CustomError, InvalidParametersError, RpcError, ThrowableData}
import co.topl.attestation.Address
import co.topl.modifier.box._
import co.topl.nodeView.state.State
import co.topl.nodeView.{
  GetHistoryFailureException,
  GetMempoolFailureException,
  GetStateFailureException,
  NodeViewHolderInterface
}
import co.topl.rpc.ToplRpc
import co.topl.settings.AppContext
import co.topl.utils.Int128
import co.topl.utils.NetworkType.NetworkPrefix
import io.circe.Encoder

import scala.concurrent.{ExecutionContext, Future}
import scala.language.existentials

class NodeViewRpcHandlerImpls(
  appContext:              AppContext,
  nodeViewHolderInterface: NodeViewHolderInterface
)(implicit
  ec:               ExecutionContext,
  throwableEncoder: Encoder[ThrowableData],
  networkPrefix:    NetworkPrefix
) extends ToplRpcHandlers.NodeView {

  override val head: ToplRpc.NodeView.Head.rpc.ServerHandler =
    _ =>
      for {
        history <- currentHistory()
      } yield ToplRpc.NodeView.Head.Response(
        history.height,
        history.score,
        history.bestBlockId,
        history.bestBlock
      )

  override val balances: ToplRpc.NodeView.Balances.rpc.ServerHandler =
    params =>
      for {
        state     <- currentState()
        addresses <- checkAddresses(params.addresses, state).toEitherT[Future]
      } yield balancesResponse(state, addresses)

  override val transactionById: ToplRpc.NodeView.TransactionById.rpc.ServerHandler =
    params =>
      for {
        history <- currentHistory()
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
        history <- currentHistory()
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
        history <- currentHistory()
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
        pool <- currentMempool()
        transactions = pool.take(100)(-_.dateAdded).map(_.tx).toList
      } yield transactions

  override val transactionFromMempool: ToplRpc.NodeView.TransactionFromMempool.rpc.ServerHandler =
    params =>
      for {
        pool <- currentMempool()
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
        ToplRpc.NodeView.Balances.EntryBoxes(
          boxes.getOrElse(PolyBox.typeString, Nil).collect { case p: PolyBox => p },
          boxes.getOrElse(ArbitBox.typeString, Nil).collect { case p: ArbitBox => p },
          boxes.getOrElse(AssetBox.typeString, Nil).collect { case p: AssetBox => p },
        )
      )
    }
  }

  private def currentHistory() =
    nodeViewHolderInterface.getHistory().leftMap { case GetHistoryFailureException(throwable) =>
      CustomError.fromThrowable(throwable): RpcError
    }

  private def currentState() =
    nodeViewHolderInterface.getState().leftMap { case GetStateFailureException(throwable) =>
      CustomError.fromThrowable(throwable): RpcError
    }

  private def currentMempool() =
    nodeViewHolderInterface.getMempool().leftMap { case GetMempoolFailureException(throwable) =>
      CustomError.fromThrowable(throwable): RpcError
    }
}

package co.topl.rpc.handlers

import akka.actor.typed.ActorSystem
import cats.data.EitherT
import cats.implicits._
import co.topl.akkahttprpc.{CustomError, InvalidParametersError, RpcError, ThrowableData}
import co.topl.attestation.Address
import co.topl.consensus.{blockVersion, getProtocolRules, Forger, ForgerInterface}
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.box._
import co.topl.network.message.BifrostSyncInfo
import co.topl.nodeView.history.HistoryReader
import co.topl.nodeView.state.StateReader
import co.topl.nodeView.{NodeViewHolderInterface, ReadableNodeView}
import co.topl.rpc.ToplRpc.NodeView.ConfirmationStatus.TxStatus
import co.topl.rpc.{ToplRpc, ToplRpcErrors}
import co.topl.settings.{AppContext, RPCApiSettings}
import co.topl.utils.Int128
import co.topl.utils.NetworkType.NetworkPrefix
import io.circe.Encoder

import scala.concurrent.Future
import scala.language.existentials

class NodeViewRpcHandlerImpls(
  rpcSettings:             RPCApiSettings,
  appContext:              AppContext,
  nodeViewHolderInterface: NodeViewHolderInterface,
  forgerInterface:         ForgerInterface
)(implicit
  system:           ActorSystem[_],
  throwableEncoder: Encoder[ThrowableData],
  networkPrefix:    NetworkPrefix
) extends ToplRpcHandlers.NodeView {

  import system.executionContext

  override val head: ToplRpc.NodeView.Head.rpc.ServerHandler =
    _ =>
      withNodeView(view =>
        ToplRpc.NodeView.Head.Response(
          view.history.height,
          view.history.score,
          view.history.bestBlockId,
          view.history.bestBlock
        )
      )

  override val headInfo: ToplRpc.NodeView.HeadInfo.rpc.ServerHandler =
    _ =>
      withNodeView(view =>
        ToplRpc.NodeView.HeadInfo.Response(
          view.history.bestBlockId,
          view.history.height
        )
      )

  override val balances: ToplRpc.NodeView.Balances.rpc.ServerHandler =
    params =>
      withNodeView(view =>
        checkAddresses(params.addresses, view.state)
          .map(balancesResponse(view.state, _))
      ).subflatMap(identity)

  override val transactionById: ToplRpc.NodeView.TransactionById.rpc.ServerHandler =
    params =>
      withNodeView(_.history.transactionById(params.transactionId))
        .subflatMap(
          _.toRight[RpcError](InvalidParametersError.adhoc("Unable to find confirmed transaction", "modifierId"))
        )
        .map { case (tx, blockId, blockNumber) => ToplRpc.NodeView.TransactionById.Response(tx, blockNumber, blockId) }

  override val blockById: ToplRpc.NodeView.BlockById.rpc.ServerHandler =
    params =>
      withNodeView(_.history.modifierById(params.blockId))
        .subflatMap(
          _.toRight[RpcError](InvalidParametersError.adhoc("The requested block could not be found", "blockId"))
        )

  override val blocksByIds: ToplRpc.NodeView.BlocksByIds.rpc.ServerHandler =
    params =>
      withNodeView(view =>
        checkBlocksFoundWithIds(
          params.blockIds,
          params.blockIds.map(view.history.modifierById),
          rpcSettings.blockRetrievalLimit
        )
      )
        .subflatMap(identity)

  override val blocksInRange: ToplRpc.NodeView.BlocksInRange.rpc.ServerHandler =
    params =>
      for {
        headHeight <- withNodeView(_.history.height)
        range <- EitherT.fromEither[Future](
          checkHeightRange(headHeight, params.startHeight, params.endHeight, rpcSettings.blockRetrievalLimit)
        )
        blocks <- withNodeView(view => getBlocksInRange(view.history, range._1, range._2))
      } yield blocks

  override val blockIdsInRange: ToplRpc.NodeView.BlockIdsInRange.rpc.ServerHandler =
    params =>
      for {
        headHeight <- withNodeView(_.history.height)
        range <- EitherT.fromEither[Future](
          checkHeightRange(headHeight, params.startHeight, params.endHeight, rpcSettings.blockIdRetrievalLimit)
        )
        ids <- withNodeView(view => getBlockIdsInRange(view.history, range._1, range._2))
      } yield ids

  override val latestBlocks: ToplRpc.NodeView.LatestBlocks.rpc.ServerHandler =
    params =>
      for {
        headHeight <- withNodeView(_.history.height)
        range <- EitherT.fromEither[Future](
          checkHeightRange(
            headHeight,
            headHeight - params.numberOfBlocks + 1,
            headHeight,
            rpcSettings.blockRetrievalLimit
          )
        )
        blocks <- withNodeView(view => getBlocksInRange(view.history, range._1, range._2))
      } yield blocks

  override val latestBlockIds: ToplRpc.NodeView.LatestBlockIds.rpc.ServerHandler =
    params =>
      for {
        headHeight <- withNodeView(_.history.height)
        range <- EitherT.fromEither[Future](
          checkHeightRange(
            headHeight,
            headHeight - params.numberOfBlockIds + 1,
            headHeight,
            rpcSettings.blockIdRetrievalLimit
          )
        )
        ids <- withNodeView(view => getBlockIdsInRange(view.history, range._1, range._2))
      } yield ids

  override val blockByHeight: ToplRpc.NodeView.BlockByHeight.rpc.ServerHandler =
    params =>
      withNodeView(_.history.modifierByHeight(params.height))
        .subflatMap(
          _.toRight[RpcError](InvalidParametersError.adhoc("The requested block could not be found", "height"))
        )

  override val mempool: ToplRpc.NodeView.Mempool.rpc.ServerHandler =
    _ => withNodeView(_.memPool.take(rpcSettings.txRetrievalLimit)(-_.dateAdded).map(_.tx).toList)

  override val transactionFromMempool: ToplRpc.NodeView.TransactionFromMempool.rpc.ServerHandler =
    params =>
      withNodeView(_.memPool.modifierById(params.transactionId))
        .subflatMap(
          _.toRight[RpcError](InvalidParametersError.adhoc("Unable to retrieve transaction", "transactionId"))
        )

  override val confirmationStatus: ToplRpc.NodeView.ConfirmationStatus.rpc.ServerHandler =
    params =>
      withNodeView { view =>
        checkTxIds(getConfirmationStatus(params.transactionIds, view.history.height, view))
      }.subflatMap(identity)

  override val info: ToplRpc.NodeView.Info.rpc.ServerHandler =
    _ =>
      withNodeView { view =>
        ToplRpc.NodeView.Info.Response(
          appContext.networkType.toString,
          appContext.externalNodeAddress.fold("N/A")(_.toString),
          appContext.settings.application.version.toString,
          getProtocolRules(view.history.height).version.toString,
          blockVersion(view.history.height).toString
        )
      }

  override val status: ToplRpc.NodeView.Status.rpc.ServerHandler =
    _ =>
      for {
        forgerStatus <- forgerInterface
          .checkForgerStatus()
          .leftMap(e => ToplRpcErrors.genericFailure(e.toString): RpcError)
          .map {
            case Forger.Active        => "active"
            case Forger.Idle          => "idle"
            case Forger.Uninitialized => "uninitialized"
          }
        mempoolSize <- withNodeView(_.memPool.size)
      } yield ToplRpc.NodeView.Status.Response(forgerStatus, mempoolSize)

  private def balancesResponse(
    state:     StateReader[_, Address],
    addresses: List[Address]
  ): ToplRpc.NodeView.Balances.Response = {
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
          boxes.getOrElse(AssetBox.typeString, Nil).collect { case p: AssetBox => p }
        )
      )
    }
  }

  private def getBlockIdsInRange(
    view:        HistoryReader[Block, BifrostSyncInfo],
    startHeight: Long,
    endHeight:   Long
  ): ToplRpc.NodeView.BlockIdsInRange.Response =
    (startHeight to endHeight)
      .flatMap(view.idAtHeightOf)
      .toList

  private def getBlocksInRange(
    view:        HistoryReader[Block, BifrostSyncInfo],
    startHeight: Long,
    endHeight:   Long
  ): ToplRpc.NodeView.BlocksInRange.Response =
    (startHeight to endHeight)
      .flatMap(view.modifierByHeight)
      .toList

  private def getConfirmationStatus(
    txIds:      List[ModifierId],
    headHeight: Long,
    view:       ReadableNodeView
  ): List[Option[(ModifierId, TxStatus)]] =
    txIds.map { id =>
      (view.memPool.modifierById(id), view.history.transactionById(id)) match {
        case (_, Some((tx, _, height))) => Some(tx.id -> TxStatus("Confirmed", headHeight - height))
        case (Some(tx), None)           => Some(tx.id -> TxStatus("Unconfirmed", -1))
        case (None, None)               => None
      }
    }

  private def withNodeView[T](f: ReadableNodeView => T) =
    nodeViewHolderInterface
      .withNodeView(f)
      .leftMap { case NodeViewHolderInterface.ReadFailure(throwable) =>
        CustomError.fromThrowable(throwable): RpcError
      }
}

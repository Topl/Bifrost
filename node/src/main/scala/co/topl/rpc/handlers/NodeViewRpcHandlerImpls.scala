package co.topl.rpc.handlers

import akka.actor.typed.ActorSystem
import cats.data.EitherT
import cats.implicits._
import co.topl.akkahttprpc.{InvalidParametersError, RpcError, ThrowableData}
import co.topl.attestation.Address
import co.topl.consensus.{blockVersion, getProtocolRules}
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.box._
import co.topl.modifier.transaction.Transaction
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
  nodeViewHolderInterface: NodeViewHolderInterface
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
      getTxsByIds(List(params.transactionId)).subflatMap {
        case (tx, blockId, blockNumber) :: Nil =>
          Either.right(ToplRpc.NodeView.TransactionById.Response(tx, blockNumber, blockId))
        case _ => Either.left(ToplRpcErrors.NoTransactionWithId)
      }

  override val blockById: ToplRpc.NodeView.BlockById.rpc.ServerHandler =
    params => getBlocksByIds(List(params.blockId)).map(_.head)

  override val blocksByIds: ToplRpc.NodeView.BlocksByIds.rpc.ServerHandler =
    params => getBlocksByIds(params.blockIds)

  override val blocksInRange: ToplRpc.NodeView.BlocksInRange.rpc.ServerHandler =
    params =>
      for {
        headHeight <- withNodeView(_.history.height)
        range <- checkHeightRange(headHeight, params.startHeight, params.endHeight, rpcSettings.blockRetrievalLimit)
          .toEitherT[Future]
        blocks <- withNodeView(view => getBlocksInRange(view.history, range._1, range._2))
      } yield blocks

  override val blockIdsInRange: ToplRpc.NodeView.BlockIdsInRange.rpc.ServerHandler =
    params =>
      for {
        headHeight <- withNodeView(_.history.height)
        range <- checkHeightRange(headHeight, params.startHeight, params.endHeight, rpcSettings.blockIdRetrievalLimit)
          .toEitherT[Future]
        ids <- withNodeView(view => getBlockIdsInRange(view.history, range._1, range._2))
      } yield ids

  override val latestBlocks: ToplRpc.NodeView.LatestBlocks.rpc.ServerHandler =
    params =>
      for {
        headHeight <- withNodeView(_.history.height)
        // Since the block at headHeight is included, we will get more than numberOfBlocks blocks without adding 1 here
        startHeight = headHeight - params.numberOfBlocks + 1
        range <- checkHeightRange(headHeight, startHeight, headHeight, rpcSettings.blockRetrievalLimit)
          .toEitherT[Future]
        blocks <- withNodeView(view => getBlocksInRange(view.history, range._1, range._2))
      } yield blocks

  override val latestBlockIds: ToplRpc.NodeView.LatestBlockIds.rpc.ServerHandler =
    params =>
      for {
        headHeight <- withNodeView(_.history.height)
        startHeight = headHeight - params.numberOfBlockIds + 1
        range <-
          checkHeightRange(headHeight, startHeight, headHeight, rpcSettings.blockIdRetrievalLimit).toEitherT[Future]

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
      for {
        txIds     <- checkModifierIdType(Transaction.modifierTypeId, List(params.transactionId)).toEitherT[Future]
        txOptions <- withNodeView(view => txIds.map(view.memPool.modifierById))
        txs       <- txOptions.sequence.toRight(ToplRpcErrors.NoTransactionWithId: RpcError).toEitherT[Future]
      } yield txs.head

  override val confirmationStatus: ToplRpc.NodeView.ConfirmationStatus.rpc.ServerHandler =
    params =>
      for {
        txIds <- EitherT.fromEither[Future](checkModifierIdType(Transaction.modifierTypeId, params.transactionIds))
        txStatusOption <- withNodeView(view => getConfirmationStatus(txIds, view.history.height, view))
        txStatus       <- txStatusOption.sequence.toRight(ToplRpcErrors.NoTransactionWithId: RpcError).toEitherT[Future]
      } yield txStatus.toMap

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

  private def getBlocksByIds(ids: List[ModifierId]): EitherT[Future, RpcError, List[Block]] =
    for {
      _            <- checkModifierRetrievalLimit(ids, rpcSettings.blockRetrievalLimit).toEitherT[Future]
      _            <- checkModifierIdType(Block.modifierTypeId, ids).toEitherT[Future]
      blockOptions <- withNodeView(view => ids.map(view.history.modifierById))
      blocks       <- blockOptions.sequence.toRight(ToplRpcErrors.NoBlockWithId: RpcError).toEitherT[Future]
    } yield blocks

  private def getTxsByIds(ids: List[ModifierId]): EitherT[Future, RpcError, List[(Transaction.TX, ModifierId, Long)]] =
    for {
      _         <- checkModifierRetrievalLimit(ids, rpcSettings.txRetrievalLimit).toEitherT[Future]
      _         <- checkModifierIdType(Transaction.modifierTypeId, ids).toEitherT[Future]
      txOptions <- withNodeView(view => ids.map(view.history.transactionById))
      txs       <- txOptions.sequence.toRight(ToplRpcErrors.NoTransactionWithId: RpcError).toEitherT[Future]
    } yield txs

  /** this function should be faster than getting the entire block out of storage and grabbing the id since the block id is stored separately */
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
        case (None, None)               => Some(id -> TxStatus("Not Found", -1))
      }
    }

  private def withNodeView[T](f: ReadableNodeView => T): EitherT[Future, RpcError, T] =
    readFromNodeViewHolder(nodeViewHolderInterface)(f)
}

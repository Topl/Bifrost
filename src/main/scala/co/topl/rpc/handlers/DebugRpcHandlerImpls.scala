package co.topl.rpc.handlers

import cats.implicits._
import co.topl.akkahttprpc.{CustomError, RpcError, ThrowableData}
import co.topl.consensus.{KeyManagerInterface, ListOpenKeyfilesFailureException}
import co.topl.modifier.ModifierId
import co.topl.nodeView.history.HistoryDebug
import co.topl.nodeView.{GetHistoryFailureException, NodeViewHolderInterface}
import co.topl.rpc.{ToplRpc, ToplRpcErrors}
import co.topl.utils.NetworkType.NetworkPrefix
import io.circe.Encoder

import scala.concurrent.{ExecutionContext, Future}

class DebugRpcHandlerImpls(
  nodeViewHolderInterface: NodeViewHolderInterface,
  keyManagerInterface:     KeyManagerInterface
)(implicit
  ec:               ExecutionContext,
  throwableEncoder: Encoder[ThrowableData],
  networkPrefix:    NetworkPrefix
) extends ToplRpcHandlers.Debug {

  override val delay: ToplRpc.Debug.Delay.rpc.ServerHandler =
    params =>
      for {
        history <- currentHistory()
        historyDebug = new HistoryDebug(history)
        delay <- historyDebug
          .averageDelay(params.blockId, params.numBlocks)
          .toEither
          .leftMap(CustomError.fromThrowable(_): RpcError)
          .toEitherT[Future]
      } yield ToplRpc.Debug.Delay.Response(s"$delay milliseconds")

  override val myBlocks: ToplRpc.Debug.MyBlocks.rpc.ServerHandler =
    _ =>
      for {
        history <- currentHistory()
        historyDebug = new HistoryDebug(history)
        myKeys <- keyManagerInterface.listOpenKeyfiles().leftMap { case ListOpenKeyfilesFailureException(throwable) =>
          CustomError.fromThrowable(throwable): RpcError
        }
        blockNum = historyDebug.count(b => myKeys.map(_.evidence).contains(b.generatorBox.evidence))
      } yield ToplRpc.Debug.MyBlocks.Response(myKeys, blockNum)

  override val generators: ToplRpc.Debug.Generators.rpc.ServerHandler =
    _ =>
      for {
        history <- currentHistory()
        historyDebug = new HistoryDebug(history)
      } yield historyDebug.forgerDistribution().map { case (key, count) =>
        key.address -> count
      }

  override val idsFromHeight: ToplRpc.Debug.IdsFromHeight.rpc.ServerHandler =
    params =>
      currentHistory().flatMap[RpcError, List[ModifierId]](
        new HistoryDebug(_)
          .getIdsFrom(params.height, params.limit)
          .toRight(ToplRpcErrors.NoBlockIdsAtHeight: RpcError)
          .toEitherT[Future]
          .map(_.toList)
      )

  private def currentHistory() =
    nodeViewHolderInterface.getHistory().leftMap { case GetHistoryFailureException(throwable) =>
      CustomError.fromThrowable(throwable): RpcError
    }
}

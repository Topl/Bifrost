package co.topl.rpc.handlers

import cats.data.EitherT
import cats.implicits._
import co.topl.akkahttprpc.{CustomError, RpcError, ThrowableData}
import co.topl.modifier.ModifierId
import co.topl.nodeView.history.HistoryDebug
import co.topl.rpc.{ToplRpc, ToplRpcErrors}
import co.topl.utils.NetworkType.NetworkPrefix
import io.circe.Encoder

import scala.concurrent.{ExecutionContext, Future}

class DebugRpcHandlerImpls(
  getHistory: GetHistory,
  listKeys:   ListKeys
)(implicit
  ec:               ExecutionContext,
  throwableEncoder: Encoder[ThrowableData],
  networkPrefix:    NetworkPrefix
) extends ToplRpcHandlers.Debug {

  override val delay: ToplRpc.Debug.Delay.rpc.ServerHandler =
    params =>
      for {
        history <- getHistory()
        historyDebug = new HistoryDebug(history)
        delay <- EitherT.fromEither[Future](
          historyDebug
            .averageDelay(params.blockId, params.numBlocks)
            .toEither
            .leftMap(CustomError.fromThrowable(-32099, "Unexpected server error", _): RpcError[_])
        )
      } yield ToplRpc.Debug.Delay.Response(s"$delay milliseconds")

  override val myBlocks: ToplRpc.Debug.MyBlocks.rpc.ServerHandler =
    _ =>
      for {
        history <- getHistory()
        historyDebug = new HistoryDebug(history)
        myKeys <- listKeys()
        blockNum = historyDebug.count(b => myKeys.map(_.evidence).contains(b.generatorBox.evidence))
      } yield ToplRpc.Debug.MyBlocks.Response(myKeys, blockNum)

  override val generators: ToplRpc.Debug.Generators.rpc.ServerHandler =
    _ =>
      for {
        history <- getHistory()
        historyDebug = new HistoryDebug(history)
      } yield historyDebug.forgerDistribution().map { case (key, count) =>
        key.address -> count
      }

  override val idsFromHeight: ToplRpc.Debug.IdsFromHeight.rpc.ServerHandler =
    params =>
      for {
        history <- getHistory()
        historyDebug = new HistoryDebug(history)
        ids <- EitherT
          .fromOption[Future]
          .apply[RpcError[_], Seq[ModifierId]](
            historyDebug.getIdsFrom(params.height, params.limit),
            ToplRpcErrors.NoBlockIdsAtHeight: RpcError[_]
          )
      } yield ids
}

package co.topl.http.rpc

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import cats.data.EitherT
import cats.implicits._
import co.topl.akkahttprpc.{CustomError, RpcError, ThrowableData}
import co.topl.attestation.Address
import co.topl.nodeView.CurrentView
import co.topl.nodeView.history.{History, HistoryDebug}
import co.topl.nodeView.mempool.MemPool
import co.topl.nodeView.state.State
import co.topl.settings.AppContext
import co.topl.utils.NetworkType.NetworkPrefix
import io.circe.Encoder

import scala.concurrent.{ExecutionContext, Future}

case class BifrostRpcHandlers(
  debug: BifrostRpcHandlers.Debug
)

object BifrostRpcHandlers {

  trait Debug {
    def delay: BifrostRpc.Debug.Delay.rpc.Handler
    def myBlocks: BifrostRpc.Debug.MyBlocks.rpc.Handler
    def generators: BifrostRpc.Debug.Generators.rpc.Handler
    def idsFromHeight: BifrostRpc.Debug.IdsFromHeight.rpc.Handler
  }
}

object BifrostRpcHandlerImpls {

  class Debug(appContext: AppContext, nodeViewHolderRef: ActorRef, forgerRef: ActorRef)(implicit
    ec:                   ExecutionContext,
    timeout:              Timeout,
    throwableEncoder:     Encoder[ThrowableData]
  ) extends BifrostRpcHandlers.Debug {

    import co.topl.consensus.Forger
    import co.topl.nodeView.NodeViewHolder

    implicit val networkPrefix: NetworkPrefix = appContext.networkType.netPrefix

    type CV = CurrentView[History, State, MemPool]

    private def currentView(): EitherT[Future, RpcError[_], CV] =
      EitherT.liftF((nodeViewHolderRef ? NodeViewHolder.ReceivableMessages.GetDataFromCurrentView).mapTo[CV])

    private def listKeys(): EitherT[Future, RpcError[_], Set[Address]] =
      EitherT.liftF((forgerRef ? Forger.ReceivableMessages.ListKeys).mapTo[Set[Address]])

    override val delay: BifrostRpc.Debug.Delay.rpc.Handler =
      params =>
        for {
          view <- currentView()
          historyDebug = new HistoryDebug(view.history)
          delay <- EitherT.fromEither[Future](
            historyDebug
              .averageDelay(params.blockId, params.numBlocks)
              .toEither
              .leftMap(CustomError.fromThrowable(-32099, "Unexpected server error", _): RpcError[_])
          )
        } yield BifrostRpc.Debug.Delay.Response(s"$delay milliseconds")

    override val myBlocks: BifrostRpc.Debug.MyBlocks.rpc.Handler =
      _ =>
        for {
          view <- currentView()
          historyDebug = new HistoryDebug(view.history)
          myKeys <- listKeys()
          blockNum = historyDebug.count(b => myKeys.map(_.evidence).contains(b.generatorBox.evidence))
        } yield BifrostRpc.Debug.MyBlocks.Response(myKeys, blockNum)

    override val generators: BifrostRpc.Debug.Generators.rpc.Handler =
      _ =>
        for {
          view <- currentView()
          historyDebug = new HistoryDebug(view.history)
        } yield historyDebug.forgerDistribution().map { case (key, count) =>
          key.address -> count
        }

    override def idsFromHeight: BifrostRpc.Debug.IdsFromHeight.rpc.Handler =
      params =>
        for {
          view <- currentView()
          historyDebug = new HistoryDebug(view.history)
          ids <- EitherT.fromOption[Future](
            historyDebug.getIdsFrom(params.height, params.limit),
            CustomError(-32005, "No block ids found from that block height", None): RpcError[_]
          )
        } yield ids
  }
}

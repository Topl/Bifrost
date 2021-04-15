package co.topl.rpc

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import cats.data.NonEmptyChain
import co.topl.akkahttprpc.implicits.server.rpcToServer
import co.topl.akkahttprpc.{MethodNotFoundError, RpcDirectives, RpcErrorRejection, ThrowableData}
import co.topl.rpc.handlers.ToplRpcHandlers
import co.topl.settings._
import co.topl.utils.NetworkType.NetworkPrefix
import io.circe._

class ToplRpcServer(handlers: ToplRpcHandlers, appContext: AppContext)(implicit
  throwableEncoder:           Encoder[ThrowableData]
) extends RpcDirectives
    with ToplRpcServerCodecs {

  implicit val networkPrefix: NetworkPrefix = appContext.networkType.netPrefix

  val debugRoutes: Option[NonEmptyChain[Route]] =
    if (appContext.settings.rpcApi.namespaceSelector.namespaceStates(DebugNamespace))
      Some(
        NonEmptyChain(
          ToplRpc.Debug.Delay.rpc.serve(handlers.debug.delay),
          ToplRpc.Debug.MyBlocks.rpc.serve(handlers.debug.myBlocks),
          ToplRpc.Debug.Generators.rpc.serve(handlers.debug.generators),
          ToplRpc.Debug.IdsFromHeight.rpc.serve(handlers.debug.idsFromHeight)
        )
      )
    else None

  val utilRoutes: Option[NonEmptyChain[Route]] =
    if (appContext.settings.rpcApi.namespaceSelector.namespaceStates(UtilNamespace)) {
      Some(
        NonEmptyChain(
          ToplRpc.Util.Seed.rpc.serve(handlers.utils.seed),
          ToplRpc.Util.SeedOfLength.rpc.serve(handlers.utils.seedOfLength),
          ToplRpc.Util.CheckValidAddress.rpc.serve(handlers.utils.checkValidAddress),
          ToplRpc.Util.GenerateAssetCode.rpc.serve(handlers.utils.generateAssetCode),
          ToplRpc.Util.HashBlake2b256.rpc.serve(handlers.utils.hashBlake2b256)
        )
      )
    } else None

  val nodeViewRoutes: Option[NonEmptyChain[Route]] =
    if (appContext.settings.rpcApi.namespaceSelector.namespaceStates(ToplNamespace)) {
      Some(
        NonEmptyChain(
          ToplRpc.NodeView.Head.rpc.serve(handlers.nodeView.head),
          ToplRpc.NodeView.Balances.rpc.serve(handlers.nodeView.balances),
          ToplRpc.NodeView.TransactionById.rpc.serve(handlers.nodeView.transactionById),
          ToplRpc.NodeView.BlockById.rpc.serve(handlers.nodeView.blockById),
          ToplRpc.NodeView.BlockByHeight.rpc.serve(handlers.nodeView.blockByHeight),
          ToplRpc.NodeView.Mempool.rpc.serve(handlers.nodeView.mempool),
          ToplRpc.NodeView.TransactionFromMempool.rpc.serve(handlers.nodeView.transactionFromMempool),
          ToplRpc.NodeView.Info.rpc.serve(handlers.nodeView.info)
        )
      )
    } else None

  val transactionRoutes: Option[NonEmptyChain[Route]] =
    if (appContext.settings.rpcApi.namespaceSelector.namespaceStates(ToplNamespace)) {
      Some(
        NonEmptyChain(
          ToplRpc.Transaction.RawAssetTransfer.rpc.serve(handlers.transaction.rawAssetTransfer),
          ToplRpc.Transaction.RawArbitTransfer.rpc.serve(handlers.transaction.rawArbitTransfer),
          ToplRpc.Transaction.RawPolyTransfer.rpc.serve(handlers.transaction.rawPolyTransfer),
          ToplRpc.Transaction.BroadcastTx.rpc.serve(handlers.transaction.broadcastTx)
        )
      )
    } else None

  val adminRoutes: Option[NonEmptyChain[Route]] =
    if (appContext.settings.rpcApi.namespaceSelector.namespaceStates(AdminNamespace)) {
      Some(
        NonEmptyChain(
          ToplRpc.Admin.UnlockKeyfile.rpc.serve(handlers.admin.unlockKeyfile),
          ToplRpc.Admin.LockKeyfile.rpc.serve(handlers.admin.lockKeyfile),
          ToplRpc.Admin.GenerateKeyfile.rpc.serve(handlers.admin.generateKeyfile),
          ToplRpc.Admin.ImportSeedPhrase.rpc.serve(handlers.admin.importSeedPhrase),
          ToplRpc.Admin.ListOpenKeyfiles.rpc.serve(handlers.admin.listOpenKeyfiles),
          ToplRpc.Admin.StartForging.rpc.serve(handlers.admin.startForging),
          ToplRpc.Admin.StopForging.rpc.serve(handlers.admin.stopForging),
          ToplRpc.Admin.UpdateRewardsAddress.rpc.serve(handlers.admin.updateRewardsAddress),
          ToplRpc.Admin.GetRewardsAddress.rpc.serve(handlers.admin.getRewardsAddress)
        )
      )
    } else None

  val route: Route =
    handleRejections(rejectionHandler)(
      NonEmptyChain.fromSeq(
        (debugRoutes ++ utilRoutes ++ nodeViewRoutes ++ transactionRoutes ++ adminRoutes).toList
          .flatMap(_.toNonEmptyList.toList)
      ) match {
        case Some(chain) =>
          chain.reduceLeft(_ ~ _)
        case _ =>
          rpcContext { context =>
            reject(RpcErrorRejection(MethodNotFoundError(context.method)))
          }
      }
    )

}

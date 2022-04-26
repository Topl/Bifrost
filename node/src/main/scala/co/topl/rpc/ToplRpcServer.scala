package co.topl.rpc

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import cats.implicits._
import co.topl.akkahttprpc.{RpcDirectives, RpcServer, ThrowableData}
import co.topl.rpc.handlers.ToplRpcHandlers
import co.topl.settings._
import co.topl.utils.NetworkType.NetworkPrefix
import io.circe._

class ToplRpcServer(handlers: ToplRpcHandlers, appContext: AppContext)(implicit
  throwableEncoder:           Encoder[ThrowableData]
) extends RpcDirectives
    with ToplRpcServerCodecs {

  implicit val networkPrefix: NetworkPrefix = appContext.networkType.netPrefix

  val debugRoutes: RpcServer.Builder =
    if (appContext.settings.rpcApi.namespaceSelector.namespaceStates(DebugNamespace)) {
      RpcServer.Builder.empty
        .append(ToplRpc.Debug.Delay.rpc)(handlers.debug.delay)
        .append(ToplRpc.Debug.MyBlocks.rpc)(handlers.debug.myBlocks)
        .append(ToplRpc.Debug.Generators.rpc)(handlers.debug.generators)
        .append(ToplRpc.Debug.IdsFromHeight.rpc)(handlers.debug.idsFromHeight)
    } else RpcServer.Builder.empty

  val utilRoutes: RpcServer.Builder =
    if (appContext.settings.rpcApi.namespaceSelector.namespaceStates(UtilNamespace)) {
      RpcServer.Builder.empty
        .append(ToplRpc.Util.Seed.rpc)(handlers.utils.seed)
        .append(ToplRpc.Util.SeedOfLength.rpc)(handlers.utils.seedOfLength)
        .append(ToplRpc.Util.CheckValidAddress.rpc)(handlers.utils.checkValidAddress)
        .append(ToplRpc.Util.GenerateAssetCode.rpc)(handlers.utils.generateAssetCode)
        .append(ToplRpc.Util.HashBlake2b256.rpc)(handlers.utils.hashBlake2b256)
    } else RpcServer.Builder.empty

  val nodeViewRoutes: RpcServer.Builder =
    if (appContext.settings.rpcApi.namespaceSelector.namespaceStates(ToplNamespace)) {
      RpcServer.Builder.empty
        .append(ToplRpc.NodeView.Head.rpc)(handlers.nodeView.head)
        .append(ToplRpc.NodeView.HeadInfo.rpc)(handlers.nodeView.headInfo)
        .append(ToplRpc.NodeView.Balances.rpc)(handlers.nodeView.balances)
        .append(ToplRpc.NodeView.TransactionById.rpc)(handlers.nodeView.transactionById)
        .append(ToplRpc.NodeView.BlockById.rpc)(handlers.nodeView.blockById)
        .append(ToplRpc.NodeView.BlocksByIds.rpc)(handlers.nodeView.blocksByIds)
        .append(ToplRpc.NodeView.BlockByHeight.rpc)(handlers.nodeView.blockByHeight)
        .append(ToplRpc.NodeView.BlocksInRange.rpc)(handlers.nodeView.blocksInRange)
        .append(ToplRpc.NodeView.BlockIdsInRange.rpc)(handlers.nodeView.blockIdsInRange)
        .append(ToplRpc.NodeView.LatestBlocks.rpc)(handlers.nodeView.latestBlocks)
        .append(ToplRpc.NodeView.LatestBlockIds.rpc)(handlers.nodeView.latestBlockIds)
        .append(ToplRpc.NodeView.Mempool.rpc)(handlers.nodeView.mempool)
        .append(ToplRpc.NodeView.TransactionFromMempool.rpc)(handlers.nodeView.transactionFromMempool)
        .append(ToplRpc.NodeView.ConfirmationStatus.rpc)(handlers.nodeView.confirmationStatus)
        .append(ToplRpc.NodeView.Info.rpc)(handlers.nodeView.info)
    } else RpcServer.Builder.empty

  val transactionRoutes: RpcServer.Builder =
    if (appContext.settings.rpcApi.namespaceSelector.namespaceStates(ToplNamespace)) {
      RpcServer.Builder.empty
        .append(ToplRpc.Transaction.RawAssetTransfer.rpc)(handlers.transaction.rawAssetTransfer)
        .append(ToplRpc.Transaction.RawArbitTransfer.rpc)(handlers.transaction.rawArbitTransfer)
        .append(ToplRpc.Transaction.RawPolyTransfer.rpc)(handlers.transaction.rawPolyTransfer)
        .append(ToplRpc.Transaction.UnprovenPolyTransfer.rpc)(handlers.transaction.unprovenPolyTransfer)
        .append(ToplRpc.Transaction.UnprovenArbitTransfer.rpc)(handlers.transaction.unprovenArbitTransfer)
        .append(ToplRpc.Transaction.BroadcastTx.rpc)(handlers.transaction.broadcastTx)
        .append(ToplRpc.Transaction.BroadcastTetraTransfer.rpc)(handlers.transaction.broadcastTetraTransfer)
        .append(ToplRpc.Transaction.EncodeTransfer.rpc)(handlers.transaction.encodeTransfer)
    } else RpcServer.Builder.empty

  val adminRoutes: RpcServer.Builder =
    if (appContext.settings.rpcApi.namespaceSelector.namespaceStates(AdminNamespace)) {
      RpcServer.Builder.empty
        .append(ToplRpc.Admin.UnlockKeyfile.rpc)(handlers.admin.unlockKeyfile)
        .append(ToplRpc.Admin.LockKeyfile.rpc)(handlers.admin.lockKeyfile)
        .append(ToplRpc.Admin.GenerateKeyfile.rpc)(handlers.admin.generateKeyfile)
        .append(ToplRpc.Admin.ImportSeedPhrase.rpc)(handlers.admin.importSeedPhrase)
        .append(ToplRpc.Admin.ListOpenKeyfiles.rpc)(handlers.admin.listOpenKeyfiles)
        .append(ToplRpc.Admin.StartForging.rpc)(handlers.admin.startForging)
        .append(ToplRpc.Admin.StopForging.rpc)(handlers.admin.stopForging)
        .append(ToplRpc.Admin.UpdateRewardsAddress.rpc)(handlers.admin.updateRewardsAddress)
        .append(ToplRpc.Admin.GetRewardsAddress.rpc)(handlers.admin.getRewardsAddress)
        .append(ToplRpc.Admin.Status.rpc)(handlers.admin.status)
    } else RpcServer.Builder.empty

  val route: Route =
    handleRejections(rejectionHandler)(
      debugRoutes.combine(utilRoutes).combine(nodeViewRoutes).combine(transactionRoutes).combine(adminRoutes).route
    )

}

package co.topl.http.rpc

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import cats.data.NonEmptyChain
import co.topl.akkahttprpc.implicits.server.rpcToServer
import co.topl.akkahttprpc.{MethodNotFoundError, RpcDirectives, RpcErrorRejection, ThrowableData}
import co.topl.http.api.{DebugNamespace, UtilNamespace}
import co.topl.settings.AppContext
import co.topl.utils.NetworkType.NetworkPrefix
import io.circe._

class ToplRpcServer(handlers: BifrostRpcHandlers, appContext: AppContext)(implicit
  throwableEncoder:           Encoder[ThrowableData]
) extends RpcDirectives
    with ToplRpcServerCodecs {

  implicit val networkPrefix: NetworkPrefix = appContext.networkType.netPrefix

  val debugRoutes: List[Route] =
    if (appContext.settings.rpcApi.namespaceSelector.namespaceStates(DebugNamespace))
      List(
        ToplRpc.Debug.Delay.rpc.serve(handlers.debug.delay),
        ToplRpc.Debug.MyBlocks.rpc.serve(handlers.debug.myBlocks),
        ToplRpc.Debug.Generators.rpc.serve(handlers.debug.generators),
        ToplRpc.Debug.IdsFromHeight.rpc.serve(handlers.debug.idsFromHeight)
      )
    else Nil

  val utilRoutes: List[Route] =
    if (appContext.settings.rpcApi.namespaceSelector.namespaceStates(UtilNamespace))
      List(
        ToplRpc.Utils.Seed.rpc.serve(handlers.utils.seed),
        ToplRpc.Utils.SeedOfLength.rpc.serve(handlers.utils.seedOfLength),
        ToplRpc.Utils.CheckValidAddress.rpc.serve(handlers.utils.checkValidAddress),
        ToplRpc.Utils.GenerateAssetCode.rpc.serve(handlers.utils.generateAssetCode),
        ToplRpc.Utils.HashBlake2b256.rpc.serve(handlers.utils.hashBlake2b256)
      )
    else Nil

  val route: Route =
    handleRejections(rejectionHandler)(
      NonEmptyChain.fromSeq(debugRoutes ++ utilRoutes) match {
        case Some(chain) =>
          chain.reduceLeft(_ ~ _)
        case _ =>
          rpcContext { context =>
            reject(RpcErrorRejection(MethodNotFoundError(context.method)))
          }
      }
    )

}

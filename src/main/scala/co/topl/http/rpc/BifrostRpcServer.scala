package co.topl.http.rpc

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import cats.data.NonEmptyChain
import co.topl.akkahttprpc.{MethodNotFoundError, RpcDirectives, RpcErrorRejection, ThrowableData}
import co.topl.http.api.DebugNamespace
import co.topl.settings.AppContext
import io.circe._

class BifrostRpcServer(handlers: BifrostRpcHandlers, appContext: AppContext)(implicit
  throwableEncoder:              Encoder[ThrowableData]
) extends RpcDirectives
    with BifrostRpcEncoders
    with BifrostRpcDecoders {

  val debugRoutes: List[Route] =
    if (appContext.settings.rpcApi.namespaceSelector.namespaceStates(DebugNamespace))
      List(
        BifrostRpc.Debug.Delay.rpc.serve(handlers.debug.delay),
        BifrostRpc.Debug.MyBlocks.rpc.serve(handlers.debug.myBlocks),
        BifrostRpc.Debug.Generators.rpc.serve(handlers.debug.generators),
        BifrostRpc.Debug.IdsFromHeight.rpc.serve(handlers.debug.idsFromHeight)
      )
    else Nil

  val route: Route =
    handleRejections(rejectionHandler)(
      NonEmptyChain.fromSeq(debugRoutes) match {
        case Some(chain) =>
          chain.reduceLeft(_ ~ _)
        case _ =>
          rpcContext { context =>
            reject(RpcErrorRejection(MethodNotFoundError(context.method)))
          }
      }
    )

}

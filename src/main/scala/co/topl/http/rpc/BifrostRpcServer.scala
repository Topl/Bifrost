package co.topl.http.rpc

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import cats.data.NonEmptyChain
import co.topl.akkahttprpc.RpcErrorCodecs.ThrowableSupport
import co.topl.akkahttprpc.{RpcDirectives, ThrowableData}
import io.circe._

class BifrostRpcServer(handlers: BifrostRpcHandlers, verboseResponses: Boolean)
    extends RpcDirectives
    with BifrostRpcEncoders
    with BifrostRpcDecoders {

  implicit val throwableEncoder: Encoder[ThrowableData] =
    ThrowableSupport.verbose(verboseResponses)

  val route: Route =
    Route.seal(
      NonEmptyChain(
        BifrostRpc.Debug.Delay.rpc.serve(handlers.debug.delay),
        BifrostRpc.Debug.MyBlocks.rpc.serve(handlers.debug.myBlocks),
        BifrostRpc.Debug.Generators.rpc.serve(handlers.debug.generators),
        BifrostRpc.Debug.IdsFromHeight.rpc.serve(handlers.debug.idsFromHeight)
      ).reduceLeft(_ ~ _)
    )

}

package co.topl.akkahttprpc

import scala.language.implicitConversions

package object implicits {

  object client {

    implicit def rpcToClient[Params, SuccessResponse](
      rpc: Rpc[Params, SuccessResponse]
    ): RpcClient[Params, SuccessResponse] =
      new RpcClient(rpc)
  }

  object server {

    implicit def rpcToServer[Params, SuccessResponse](
      rpc: Rpc[Params, SuccessResponse]
    ): RpcServer[Params, SuccessResponse] =
      new RpcServer(rpc)
  }
}

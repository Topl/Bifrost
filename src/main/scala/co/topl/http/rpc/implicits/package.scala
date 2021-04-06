package co.topl.http.rpc

package object implicits {
  object client extends ToplRpcClientCodecs
  object server extends ToplRpcServerCodecs
}

# topl-rpc
A library providing definitions and codecs for the Topl RPC.

## Installation
In `build.sbt`, add the following:

```scala
libraryDependencies += "co.topl" %% "topl-rpc" % VERSION
```

## Usage

### Client
1. Prepare the environment.
    ```scala
    import akka.http.scaladsl.model.headers.RawHeader
    import cats.data._
    import co.topl.akkahttprpc._
    import co.topl.akkahttprpc.implicits.client._
    import co.topl.rpc._
    import co.topl.rpc.implicits.client._
    import co.topl.utils.NetworkType
    import co.topl.utils.NetworkType.NetworkPrefix
    
   object MyToplRpcClient {
        implicit val requestModifier: RequestModifier =
          RequestModifier(_.withUri("http://localhost:8085").withHeaders(RawHeader("x-api-key", "my_api_key")))
        implicit val networkPrefix: NetworkPrefix =
          NetworkType.PrivateTestnet.netPrefix
   
        // Run RPCs here
    }
    ```

1. Call an RPC.
    ```scala
   object MyToplRpcClient {
      // ...
      def currentHeight()(implicit ec: ExecutionContext): EitherT[Future, RpcClientFailure, Int128] =
        ToplRpc.NodeView.Info.rpc(ToplRpc.NodeView.Info.Params()).map(_.height)
   }
   ```
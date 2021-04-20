# akka-http-rpc
A library for implementing JSON-RPC 2.0 servers and clients using akka-http and Cats.

## Installation
In `build.sbt`, add the following:

```scala
libraryDependencies += "co.topl" %% "akka-http-rpc" % VERSION
```

## Usage
1. Define your RPC(s).
    ```scala
    import co.topl.akkahttprpc._
   
    object MyBusinessRPC {
        object SanitizeName {
            val rpc: Rpc[Params, Response] = Rpc("sanitize_name")
            case class Params(unsanitizedName: String)
            case class Response(sanitizedName: String)
        }
    }
    ```

1. Write codecs.
    ```scala
    import io.circe._
    import io.circe.generic.semiauto._
   
    trait MyBusinessRPCCodecs {
      implicit val encodeSanitizeNameParams: Encoder[MyBusinessRPC.SanitizeName.Params] = deriveEncoder
      implicit val encodeSanitizeNameResponse: Encoder[MyBusinessRPC.SanitizeName.Response] = deriveEncoder
      implicit val decodeSanitizeNameParams: Decoder[MyBusinessRPC.SanitizeName.Params] = deriveDecoder
      implicit val decodeSanitizeNameResponse: Decoder[MyBusinessRPC.SanitizeName.Response] = deriveDecoder
    }
    ```

1. Serve the RPC(s).
    ```scala
    import akka.http.scaladsl.server._
    import cats.implicits._
    import co.topl.akkahttprpc._
    import co.topl.akkahttprpc.implicits.server._
   
    object MyBusinessRPCServer extends MyBusinessRPCCodecs {
        private def sanitizer(unsanitized: String): String = ???
        
        val route: Route =
          MyBusinessRPC.SanitizeName.rpc.serve(params =>
            MyBusinessRPC.SanitizeName.Response(sanitizer(params.unsanitizedName)).asRight[RpcError].toEitherT[Future]
          )
        
        Http().newServerAt("localhost", 8080).bind(route)
    }
    ```

1. Call the RPC(s).
    ```scala
    import cats.implicits._
    import co.topl.akkahttprpc._
    import co.topl.akkahttprpc.implicits.client._
   
    object MyBusinessRPCClient extends MyBusinessRPCCodecs {
      def sanitizeName(unsaitized: String): EitherT[Future, RpcClientFailure, MyBusinessRPC.SanitizeName.Response] =
        MyBusinessRPC.SanitizeName.rpc(MyBusinessRPC.SanitizeName.Params("He%llo World!"))
    }
    ```

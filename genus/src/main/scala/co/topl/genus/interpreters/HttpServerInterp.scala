package co.topl.genus.interpreters

import akka.actor.ActorSystem
import akka.grpc.scaladsl.ServiceHandler
import akka.http.scaladsl.model.{HttpRequest, HttpResponse}
import akka.http.scaladsl.server.Directive0
import akka.http.scaladsl.server.Directives.{handle, optionalHeaderValueByName, pass, reject}
import akka.http.scaladsl.{Http, ServerBuilder}
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.crypto.hash.blake2b256
import co.topl.genus.algebras.HttpServerAlg
import co.topl.utils.StringDataTypes.Base58Data
import co.topl.utils.implicits._

import scala.concurrent.Future

object HttpServerInterp {

  object Eval {

    /**
     * Creates an HTTP server from a set of provided handlers, an IP address, and a port.
     * @param handlers the collection of handlers to route requests to
     * @param ip the IP address to run this server on
     * @param port the port to run the server on
     * @param apiKey the Base-58 encoded Blake2b-256 hash of the expected key to be provided in the x-api-key header
     * @param system the Akka [[ActorSystem]] to manage the server with
     * @tparam F an effect-ful type representing the final value of the program
     * @return an instance of the [[HttpServerAlg]]
     */
    def make[F[_]: Async](
      handlers: PartialFunction[HttpRequest, Future[HttpResponse]]*
    )(ip:       String, port: Int, apiKey: Option[Base58Data])(implicit
      system:   ActorSystem
    ): HttpServerAlg[F] =
      new HttpServerAlg[F] {

        val servicesHandler: HttpRequest => Future[HttpResponse] =
          ServiceHandler.concatOrNotFound(handlers: _*)

        val builder: ServerBuilder = Http(system).newServerAt(ip, port)

        val withXApiKeyAuth: Directive0 =
          optionalHeaderValueByName("x-api-key").flatMap { keyOpt =>
            (keyOpt, apiKey) match {
              case (Some(provided), Some(needed)) =>
                // check that the provided key hashes to the expected value
                val hashedKey = blake2b256.hash(provided.getBytes("UTF-8"))

                if (hashedKey.value === needed.value) pass
                else reject

              case (None, Some(_)) => reject
              case _               => pass
            }
          }

        override def run: F[Http.ServerBinding] =
          Async[F]
            .fromFuture(
              builder
                .bind(
                  withXApiKeyAuth(handle(servicesHandler))
                )
                .pure[F]
            )

      }
  }
}

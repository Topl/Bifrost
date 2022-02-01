package co.topl.genus.interpreters

import akka.actor.ActorSystem
import akka.grpc.scaladsl.ServiceHandler
import akka.http.scaladsl.{Http, ServerBuilder}
import akka.http.scaladsl.model.{HttpRequest, HttpResponse}
import cats.effect.kernel.Async
import co.topl.genus.algebras.HttpServerAlg
import cats.implicits._

import scala.concurrent.Future

object HttpServerInterp {

  object Eval {

    /**
     * Creates an HTTP server from a set of provided handlers, an IP address, and a port.
     * @param handlers the collection of handlers to route requests to
     * @param ip the IP address to run this server on
     * @param port the port to run the server on
     * @param system the Akka [[ActorSystem]] to manage the server with
     * @tparam F an effect-ful type representing the final value of the program
     * @return an instance of the [[HttpServerAlg]]
     */
    def make[F[_]: Async](handlers: PartialFunction[HttpRequest, Future[HttpResponse]]*)(ip: String, port: Int)(implicit
      system:                       ActorSystem
    ): HttpServerAlg[F] =
      new HttpServerAlg[F] {

        val servicesHandler: HttpRequest => Future[HttpResponse] =
          ServiceHandler.concatOrNotFound(handlers: _*)

        val builder: ServerBuilder = Http(system).newServerAt(ip, port)

        override def run: F[Http.ServerBinding] =
          Async[F]
            .fromFuture(
              builder.bind(servicesHandler).pure[F]
            )

      }
  }
}

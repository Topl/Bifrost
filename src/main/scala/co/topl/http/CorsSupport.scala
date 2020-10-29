package co.topl.http

import akka.http.scaladsl.marshalling.ToResponseMarshallable.apply
import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.http.scaladsl.server.Directive.addByNameNullaryApply
import akka.http.scaladsl.server.{Directive0, Directives, Route}

/**
 * Provides tools for handling a Cross-Origin Resource Sharing spec workflow
 * (including `OPTIONS` pre-flight requests).
 * see https://groups.google.com/forum/#!topic/akka-user/5RCZIJt7jHo
 */
trait CorsSupport extends Directives {

  private val corsResponseHeaders: List[ModeledHeader] = List[ModeledHeader](
    `Access-Control-Allow-Origin`.*,
    `Access-Control-Allow-Credentials`(true),
    `Access-Control-Allow-Headers`("Authorization", "Content-Type", "X-Requested-With", "x-api-key")
  )

  def corsHandler(r: Route): Route = addAccessControlHeaders {
    preflightRequestHandler ~ r
  }

  private def addAccessControlHeaders: Directive0 =
    respondWithHeaders(corsResponseHeaders)

  //this handles preflight OPTIONS requests.
  private def preflightRequestHandler: Route = options {
    complete {
      HttpResponse(StatusCodes.OK)
        .withHeaders(`Access-Control-Allow-Methods`(OPTIONS, POST, PUT, GET, DELETE))
    }
  }

}

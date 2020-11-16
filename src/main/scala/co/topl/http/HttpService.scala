package co.topl.http

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.directives.RouteDirectives
import co.topl.http.api.ApiRoute

final case class HttpService(coreRoutes: Seq[ApiRoute])(implicit val system: ActorSystem)
  extends CorsSupport {

  private def coreApi: Route =
    coreRoutes.map(_.route).reduceOption(_ ~ _).getOrElse(RouteDirectives.reject)

  private def status: Route =
    (get & path("status")) {
      getFromResource("index.html")
    }

  val compositeRoute: Route =
    corsHandler {
      coreApi ~ status
    }

}

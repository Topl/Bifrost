package bifrost.http

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import bifrost.http.api.ApiRoute
import bifrost.settings.AppSettings


case class CompositeHttpService(system: ActorSystem, routes: Seq[ApiRoute], settings: AppSettings)
  extends CorsSupport {

  implicit val actorSystem: ActorSystem = system

  val redirectToStatus: Route = {
    redirect("/status", StatusCodes.PermanentRedirect)
  }

  val compositeRoute: Route = routes.map(_.route).reduce(_ ~ _) ~
    path("status") {
      getFromResource("index.html")
    } ~ redirectToStatus

}

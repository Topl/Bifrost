package bifrost.api.http

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import bifrost.api.http.swagger.CorsSupport
import bifrost.settings.Settings

import scala.reflect.runtime.universe.Type


case class CompositeHttpService(system: ActorSystem, apiTypes: Seq[Type], routes: Seq[ApiRoute], settings: Settings)
  extends CorsSupport {

  implicit val actorSystem = system

  val redirectToStatus: Route = {
    redirect("/status", StatusCodes.PermanentRedirect)
  }

  //TODO Replace with simple status page
  val compositeRoute = routes.map(_.route).reduce(_ ~ _) ~
    path("status") {
      getFromResource("swagger-ui/index.html")
    } ~ getFromResourceDirectory("swagger-ui") ~ redirectToStatus

}

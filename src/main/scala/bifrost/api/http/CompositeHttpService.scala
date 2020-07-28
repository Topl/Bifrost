package bifrost.api.http

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import bifrost.settings.Settings

import scala.reflect.runtime.universe.Type


case class CompositeHttpService(system: ActorSystem, apiTypes: Seq[Type], routes: Seq[ApiRoute], settings: Settings)
  extends CorsSupport {

  implicit val actorSystem: ActorSystem = system

  val compositeRoute: Route = routes.map(_.route).reduce(_ ~ _) ~
    path("status") {
      getFromResource("index.html")
    }

}

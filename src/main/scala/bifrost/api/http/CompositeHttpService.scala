package bifrost.api.http

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Directives._
import scorex.core.api.http._
import scorex.core.api.http.swagger.CorsSupport
import scorex.core.settings.Settings

import scala.reflect.runtime.universe.Type


case class CompositeHttpService(system: ActorSystem, apiTypes: Seq[Type], routes: Seq[ApiRoute], settings: Settings)
  extends CorsSupport {

  implicit val actorSystem = system

  val compositeRoute = routes.map(_.route).reduce(_ ~ _)

}

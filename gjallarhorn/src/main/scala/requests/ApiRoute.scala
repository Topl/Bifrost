package requests

import akka.actor.ActorRefFactory
import akka.http.scaladsl.server.Directives
import akka.util.Timeout
import http.Namespace
import io.circe.{Decoder, Json}
import settings.RPCApiSettings

import scala.concurrent.Future

trait ApiRoute extends Directives {
  val context: ActorRefFactory
  val settings: RPCApiSettings

  implicit val timeout: Timeout = Timeout(settings.timeout)

  val namespace: Namespace
  // these are the case statements for identifying the api services
  val handlers: PartialFunction[(String, Vector[Json], String), Future[Json]]

  /**
   * Helper function to parse optional parameters from the request
   * @param key optional key to be looked for
   * @param default default return value
   * @tparam A type of the value expected to be retrieved
   * @return the provided value or the default
   */
  def parseOptional[A](key: String, default: A)(implicit params: Json, decode: Decoder[A]): A =
    params.hcursor.downField(key).as[A] match {
      case Right(value) => value
      case Left(_)      => default
    }
}

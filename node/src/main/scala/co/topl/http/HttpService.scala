package co.topl.http

import akka.actor.ActorSystem
import akka.http.scaladsl.marshalling.Marshaller._
import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.http.scaladsl.server.Route
import co.topl.rpc.ToplRpcServer
import co.topl.settings.RPCApiSettings
import scorex.crypto.hash.Blake2b256
import scorex.util.encode.Base58

final case class HttpService(
  settings:            RPCApiSettings,
  bifrostRpcServer:    ToplRpcServer
)(implicit val system: ActorSystem)
    extends CorsSupport {

  private val apiKeyHash: Option[Array[Byte]] =
    Base58.decode(settings.apiKeyHash).toOption

  /** the primary route that the HTTP service is bound to in BifrostApp */
  val compositeRoute: Route =
    corsHandler {
      status ~ basicRoute
    }

  /** a static route for exposing an HTML webpage with the node status */
  private def status: Route =
    (get & path("status")) {
      getFromResource("index.html")
    }

  /** the api controller, this will parse the JSON body and target the appropriate service for handling the request */
  private def basicRoute: Route =
    path("") {
      withAuth {
        bifrostRpcServer.route
      }
    }

  /** Helper route to wrap the handling of API key authentication */
  def withAuth(route: => Route): Route =
    optionalHeaderValueByName("x-api-key") { keyOpt =>
      if (isValid(keyOpt)) route
      else complete(HttpEntity(ContentTypes.`application/json`, "Provided API key is not correct"))
    }

  /**
   * Performs the check of an incoming api key
   * @param keyOpt api key specified in header
   * @return
   */
  private def isValid(keyOpt: Option[String]): Boolean =
    apiKeyHash.forall(expected => keyOpt.map(Blake2b256(_)).exists(expected sameElements _))
}

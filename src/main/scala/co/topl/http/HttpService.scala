package co.topl.http

import akka.actor.ActorSystem
import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.http.scaladsl.server.Route
import akka.util.Timeout
import co.topl.http.api.{ApiEndpoint, ApiResponse, ErrorResponse, SuccessResponse}
import co.topl.settings.RPCApiSettings
import io.circe.Json
import io.circe.parser.parse
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.util.encode.Base58

import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

final case class HttpService (apiServices: Seq[ApiEndpoint], settings: RPCApiSettings)(implicit val system: ActorSystem)
  extends CorsSupport {

  private val timeout: Timeout = Timeout(settings.timeout)

  private lazy val apiKeyHash: Option[Array[Byte]] = Base58.decode(settings.apiKeyHash).toOption

  private val apiServiceHandlers: PartialFunction[(String, Vector[Json], String), Future[Json]] =
    apiServices
      .filter(endpoint => settings.namespaceSelector.namespaceStates(endpoint.namespace))
      .map(_.handlers)
      .reduce(_ orElse _)

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
  private def basicRoute: Route = path("") {
    entity(as[String]) { body =>
      withAuth {
        postJsonRoute {
          var reqId = ""
          parse(body) match {
            case Left(failure) => ErrorResponse(failure.getCause, 400, reqId)
            case Right(request) =>
              val futureResponse: Try[Future[Json]] = Try {
                val id = (request \\ "id").head.asString.get
                reqId = id
                require((request \\ "jsonrpc").head.asString.get == "2.0")

                val params = (request \\ "params").head.asArray.get
                require(params.size <= 1, s"size of params is ${params.size}")

                val method = (request \\ "method").head.asString.get

                if (apiServiceHandlers.isDefinedAt(method, params, id)) apiServiceHandlers.apply(method, params, id)
                else throw new Exception("Service handler not found for method: " + method)
              }

              // await result of future from handler
              futureResponse map { response =>
                Await.result(response, timeout.duration)
              } match {
                case Success(resp) => SuccessResponse(resp, reqId)
                case Failure(e) => ErrorResponse(e, 500, reqId, verbose = settings.verboseAPI)
              }
          }
        }
      }
    }
  }

  /**
   * Formats the JSON-RPC post request (this is the primary method of communication)
   * @param fn outgoing respsonse
   * @return
   */
  private def postJsonRoute(fn: ApiResponse): Route = post {
    complete(
      HttpEntity(ContentTypes.`application/json`, fn.toJson.spaces2)
    )
  }

  /** Helper route to wrap the handling of API key authentication */
  def withAuth(route: => Route): Route = {
    optionalHeaderValueByName("x-api-key") { keyOpt =>
      if (isValid(keyOpt)) route
      else complete(HttpEntity(ContentTypes.`application/json`, "Provided API key is not correct"))
    }
  }

  /**
   * Performs the check of an incoming api key
   * @param keyOpt api key specified in header
   * @return
   */
  private def isValid(keyOpt: Option[String]): Boolean = {
    lazy val keyHash: Option[Digest32] = keyOpt.map(Blake2b256(_))
    (apiKeyHash, keyHash) match {
      case (None, _) => true
      case (Some(expected), Some(passed)) => expected sameElements passed
      case _ => false
    }
  }
}

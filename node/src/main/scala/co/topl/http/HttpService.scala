package co.topl.http

import akka.actor.ActorSystem
import akka.http.scaladsl.marshalling.Marshaller
import akka.http.scaladsl.marshalling.Marshaller._
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpResponse}
import akka.http.scaladsl.server.Route
import co.topl.http.api.{ApiEndpoint, ApiResponse, ErrorResponse, SuccessResponse}
import co.topl.settings.RPCApiSettings
import io.circe.Json
import io.circe.parser.parse
import co.topl.crypto.hash.{Hash, Digest32}
import scorex.util.encode.Base58

import scala.concurrent.{Future, TimeoutException}
import scala.util.Try

final case class HttpService(apiServices: Seq[ApiEndpoint], settings: RPCApiSettings)(implicit val system: ActorSystem)
    extends CorsSupport {

  import HttpService._
  import system.dispatcher

  // use Blake2b256 hashing
  import co.topl.crypto.hash.Blake2b256._

  private lazy val apiKeyHash: Option[Array[Byte]] = Base58.decode(settings.apiKeyHash).toOption

  private val apiServiceHandlers: PartialFunction[(String, Vector[Json], String), Future[Json]] =
    apiServices
      .filter(endpoint => settings.namespaceSelector.namespaceStates(endpoint.namespace))
      .map(_.handlers)
      .fold(PartialFunction.empty[(String, Vector[Json], String), Future[Json]])(_ orElse _)

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
        complete {
          var reqId = ""
          parse(body) match {
            case Left(failure) =>
              Future.successful(
                ErrorResponse(new Exception("Unable to parse Json body"), 400, reqId, verbose = settings.verboseAPI)
              )
            case Right(request) =>
              val futureResponse: Future[ApiResponse] =
                Future
                  .fromTry(
                    Try {
                      val id = (request \\ "id").head.asString.get
                      reqId = id
                      require((request \\ "jsonrpc").head.asString.get == "2.0")

                      val params = (request \\ "params").head.asArray.get
                      require(params.size <= 1, s"size of params is ${params.size}")

                      val method = (request \\ "method").head.asString.get

                      if (apiServiceHandlers.isDefinedAt(method, params, id))
                        apiServiceHandlers.apply(method, params, id)
                      else throw new NoSuchElementException("Service handler not found for method: " + method)
                    }
                  )
                  .flatten
                  .map(SuccessResponse(_, reqId))
                  .recover { case e => ErrorResponse(e, 500, reqId, verbose = settings.verboseAPI) }
              timeoutFuture(reqId, futureResponse)
          }
        }
      }
    }
  }

  private def timeoutFuture(requestId: String, futureResponse: => Future[ApiResponse]): Future[ApiResponse] =
    Future.firstCompletedOf(
      List(
        futureResponse,
        akka.pattern.after(settings.timeout)(Future.failed(new TimeoutException))
      )
    )
      .recover {
        case e: TimeoutException =>
          ErrorResponse(e, 500, requestId, verbose = settings.verboseAPI)
      }

  /** Helper route to wrap the handling of API key authentication */
  def withAuth(route: => Route): Route =
    optionalHeaderValueByName("x-api-key") { keyOpt =>
      if (isValid(keyOpt)) route
      else complete(HttpEntity(ContentTypes.`application/json`, "Provided API key is not correct"))
    }

  /** Performs the check of an incoming api key
    * @param keyOpt api key specified in header
    * @return
    */
  private def isValid(keyOpt: Option[String]): Boolean = {
    lazy val keyHash: Option[Digest32] = keyOpt.map(Hash(_))
    (apiKeyHash, keyHash) match {
      case (None, _)                      => true
      case (Some(expected), Some(passed)) => expected sameElements passed
      case _                              => false
    }
  }
}

object HttpService {

  implicit val apiResponseMarshaller: Marshaller[ApiResponse, HttpResponse] =
    Marshaller.withFixedContentType[ApiResponse, HttpResponse](ContentTypes.`application/json`)(response =>
      HttpResponse(entity = HttpEntity(ContentTypes.`application/json`, response.toJson.spaces2))
    )

}

package requests

import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.http.scaladsl.server.{Directive0, Directives, Route}
import akka.util.Timeout
import io.circe.{Decoder, Json}
import io.circe.parser.parse
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.util.encode.Base58
import settings.AppSettings

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

trait ApiRoute extends Directives {
  val context: ActorRefFactory
  val route: Route
  val settings: AppSettings

  implicit val timeout: Timeout = Timeout(5.seconds)

  lazy val corsAllowed: Boolean = true
  lazy val apiKeyHash: Option[Array[Byte]] =
    if (settings.apiKeyHash == "") None
    else Base58.decode(settings.apiKeyHash).toOption

  def actorRefFactory: ActorRefFactory = context

  def postJsonRoute(fn: ApiResponse): Route = jsonRoute(fn, post)

  /**
    * Returns json route based on API response and method.
    * @param fn - an API response.
    * @param method
    * @return - the JSON route.
    */
  private def jsonRoute(fn: ApiResponse, method: Directive0): Route = method {
    complete(
      HttpEntity(ContentTypes.`application/json`, fn.toJson.spaces2)
    )
  }

  /**
    * Returns a route if the API key is valid.
    * @param route
    * @return
    */
  def withAuth(route: => Route): Route = {
    optionalHeaderValueByName("x-api-key") { keyOpt =>
      if (isValid(keyOpt)) route
      else complete(HttpEntity(ContentTypes.`application/json`, "Provided API key is not correct"))
    }
  }

  /**
    * Checks if a key is a valid API key.
    * @param keyOpt
    * @return - true if the key is a valid API key, false otherwise.
    */
  private def isValid(keyOpt: Option[String]): Boolean = {
    lazy val keyHash: Option[Digest32] = keyOpt.map(Blake2b256(_))
    (apiKeyHash, keyHash) match {
      case (None, _) => true
      case (Some(expected), Some(passed)) => expected sameElements passed
      case _ => false
    }
  }

  /**
    * Helper function to parse optional parameters from the request
    * @param key optional key to be looked for
    * @param default default return value
    * @tparam A type of the value expected to be retrieved
    * @return the provided value or the default
    */
  def parseOptional[A](key: String, default: A)(implicit params: Json, decode: Decoder[A]): A = {
    params.hcursor.downField(key).as[A] match {
      case Right(value) => value
      case Left(_)      => default
    }
  }


  protected final def basicRoute( handler: (String, Vector[Json], String) => Future[Json]): Route = path("") {
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

                handler(method, params, id)
              }

              // await result of future from handler
              futureResponse map { response =>
                Await.result(response, timeout.duration)
              } match {
                case Success(resp) => SuccessResponse(resp, reqId)
                case Failure(e) => ErrorResponse(e, 500, reqId)
              }
          }
        }
      }
    }
  }
}
package co.topl.http.api

import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.http.scaladsl.server.{Directive0, Directives, Route}
import akka.util.Timeout
import co.topl.settings.RESTApiSettings
import io.circe.parser.parse
import io.circe.{Decoder, Json}
import scorex.util.encode.Base58
import scorex.crypto.hash.{Blake2b256, Digest32}

import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

trait ApiRoute extends Directives {
  val settings: RESTApiSettings
  val context: ActorRefFactory
  val route: Route

  implicit val timeout: Timeout = Timeout(settings.timeout)

  lazy val corsAllowed: Boolean = settings.corsAllowed
  lazy val apiKeyHash: Option[Array[Byte]] = None

  def actorRefFactory: ActorRefFactory = context

  def postJsonRoute(fn: ApiResponse): Route = jsonRoute(fn, post)

  /**
   *
   * @param fn
   * @param method
   * @return
   */
  private def jsonRoute(fn: ApiResponse, method: Directive0): Route = method {
    complete(
      HttpEntity(ContentTypes.`application/json`, fn.toJson.spaces2)
    )
  }

  /**
   *
   * @param route
   * @return
   */
  def withAuth(route: => Route): Route = {
    optionalHeaderValueByName("x-api-key") { keyOpt =>
      if (isValid(keyOpt)) route
      else complete(HttpEntity(ContentTypes.`application/json`, s"Provided API key: ${settings.apiKeyHash} is not correct"))
    }
  }

  /**
   *
   * @param keyOpt
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

  /**
   *
   * @param handler
   * @return
   */
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
                case Failure(e) => ErrorResponse(e, 500, reqId, verbose = settings.verboseAPI)
              }
          }
        }
      }
    }
  }
}
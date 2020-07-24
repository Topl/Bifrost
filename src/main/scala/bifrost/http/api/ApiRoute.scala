package bifrost.http.api

import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.http.scaladsl.server.{Directive0, Directives, Route}
import akka.util.Timeout
import bifrost.settings.AppSettings
import io.circe.Json
import io.circe.parser.parse
import scorex.crypto.encode.Base58
import scorex.crypto.hash.{Blake2b256, CryptographicHash}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

trait ApiRoute extends Directives {
  val settings: AppSettings
  val context: ActorRefFactory
  val route: Route

  implicit val timeout: Timeout = Timeout(5.seconds)

  lazy val corsAllowed: Boolean = settings.restApi.corsAllowed
  lazy val apiKeyHash: Option[Array[Byte]] = Base58.decode(settings.restApi.apiKeyHash).toOption

  def actorRefFactory: ActorRefFactory = context

  def postJsonRoute(fn: ApiResponse): Route = jsonRoute(fn, post)

  def postJsonRoute(fn: Future[ApiResponse]): Route = jsonRoute(Await.result(fn, timeout.duration), post)

  private def jsonRoute(fn: ApiResponse, method: Directive0): Route = method {
    val resp = complete(HttpEntity(ContentTypes.`application/json`, fn.toJson.spaces2))
    withCors(resp)
  }

  def withCors(fn: => Route): Route = {
    if (corsAllowed) respondWithHeaders(RawHeader("Access-Control-Allow-Origin", "*"))(fn)
    else fn
  }

  def withAuth(route: => Route): Route = {
    optionalHeaderValueByName("x-api-key") { keyOpt =>
      if (isValid(keyOpt)) route
      else complete(HttpEntity(ContentTypes.`application/json`, "Provided API key is not correct"))
    }
  }

  private def isValid(keyOpt: Option[String]): Boolean = {
    lazy val keyHash: Option[CryptographicHash#Digest] = keyOpt.map(Blake2b256(_))
    (apiKeyHash, keyHash) match {
      case (None, _) => true
      case (Some(expected), Some(passed)) => expected sameElements passed
      case _ => false
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
                case Failure(e) => ErrorResponse(e, 500, reqId, verbose = settings.verboseAPI)
              }
          }
        }
      }
    }
  }
}
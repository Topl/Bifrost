package bifrost.api.http
import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import bifrost.history.BifrostHistory
import bifrost.mempool.BifrostMemPool
import bifrost.state.BifrostState
import bifrost.wallet.BWallet
import scorex.core.settings.Settings
import io.circe.Json
import io.circe.syntax._
import io.circe.parser.parse
import scorex.core.api.http.{ApiException, SuccessApiResponse}
import scorex.crypto.encode.Base58

import scala.util.Try
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

case class DebugApiRouteRPC (override val settings: Settings, nodeViewHolderRef: ActorRef)
                       (implicit val context: ActorRefFactory) extends ApiRouteWithView {

  type HIS = BifrostHistory
  type MS = BifrostState
  type VL = BWallet
  type MP = BifrostMemPool
  override val route: Route = pathPrefix("debugRPC") { debugRoute }

  //noinspection ScalaStyle
  def debugRoute: Route = path("") { entity(as[String]) { body =>
    withAuth {
      postJsonRoute {
        viewAsync().map {
          view =>
            var reqId = ""
            parse(body) match {
              case Left(failure) => ApiException(failure.getCause)
              case Right(json) =>
                val futureResponse: Try[Future[Json]] = Try {
                  val id = (json \\ "id").head.asString.get
                  reqId = id
                  require((json \\ "jsonrpc").head.asString.get == "2.0")
                  val params = (json \\ "params").head.asArray.get
                  require(params.size <= 5, s"size of params is ${params.size}")

                  (json \\ "method").head.asString.get match {
                    case "info" => infoRoute(params.head, id)
                  }

                }
                futureResponse map {
                  response => Await.result(response, timeout.duration)
                }
                match {
                  case Success(resp) => BifrostSuccessResponse(resp, reqId)
                  case Failure(e) => BifrostErrorResponse(e, 500, reqId, verbose = settings.settingsJSON.getOrElse("verboseAPI", false.asJson).asBoolean.get)
                }
            }
        }
      }
    }
  }

  }


  private def infoRoute(params: Json, id: String): Future[Json] = {
      viewAsync().map {
        view =>
          //SuccessApiResponse(
            Map(
              "height" -> view.history.height.toString.asJson,
              "score" -> view.history.score.asJson,
              "bestBlockId" -> Base58.encode(view.history.bestBlockId).asJson,
              "bestBlock" -> view.history.bestBlock.json,
              "stateVersion" -> Base58.encode(view.state.version).asJson
            ).asJson
      }
    }

}

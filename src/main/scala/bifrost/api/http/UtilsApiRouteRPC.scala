package bifrost.api.http

import java.security.SecureRandom

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import bifrost.history.BifrostHistory
import bifrost.mempool.BifrostMemPool
import bifrost.settings.Settings
import bifrost.state.BifrostState
import bifrost.wallet.BWallet
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax._
import scorex.crypto.encode.Base58

import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

abstract class UtilsApiRouteRPC(override val settings: Settings)
                              (implicit val context: ActorRefFactory) extends ApiRoute {
  type HIS = BifrostHistory
  type MS = BifrostState
  type VL = BWallet
  type MP = BifrostMemPool

  val SeedSize = 32

  override val route: Route = pathPrefix("utilsRPC") {
    utilsRoute
  }

  def utilsRoute: Route = path("") {
    entity(as[String]) { body =>
      withAuth {
        postJsonRoute {
//          viewAsync().map { view =>
            var reqId = ""
            parse(body) match {
              case Left(failure) => ApiException(failure.getCause)
              case Right(json) =>
                val futureResponse: Try[Json] = Try {
                  val id = (json \\ "id").head.asString.get
                  reqId = id
                  require((json \\ "jsonrpc").head.asString.get == "2.0")
                  val params = (json \\ "params").head.asArray.get
                  require(params.size <= 5, s"size of params is ${params.size}")

                  (json \\ "method").head.asString.get match {
                    case "seed" => seedRoute(params.head, id)
//                    case "seedOfLength" => seedOfLength(params.head, id)
                  }
                }
                futureResponse
                  // map {
//                  response => Await.result(response, timeout.duration)
//                }
                match {
                  case Success(resp) => BifrostSuccessResponse(resp, reqId)
                  case Failure(e) => BifrostErrorResponse(e, 500, reqId, verbose = settings.settingsJSON.getOrElse("verboseAPI", false.asJson).asBoolean.get)
                }
            }

//          }
        }
      }
    }
  }

  private def seed(length: Int): Json = {
    val seed = new Array[Byte](length)
    new SecureRandom().nextBytes(seed) //seed mutated here!
    Map("seed" -> Base58.encode(seed)).asJson
  }

  private def seedRoute(params: Json, id: String): Json = {
        seed(SeedSize)
  }

//  private def seedOfLength(params: Json, id: String): Future[Json] = {
//    viewAsync().map {
//      view =>
//        val length: Int = (params \\ "length").head.asNumber.get.toInt.get
//        seed(length)
//    }
//  }

}

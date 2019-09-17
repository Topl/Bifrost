package bifrost.api.http

import java.security.SecureRandom

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import bifrost.crypto.hash.FastCryptographicHash
import bifrost.history.BifrostHistory
import bifrost.mempool.BifrostMemPool
import bifrost.settings.Settings
import bifrost.state.BifrostState
import bifrost.wallet.BWallet
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax._
import scorex.crypto.encode.Base58

import scala.util.{Failure, Success, Try}

case class UtilsApiRoute(override val settings: Settings)
                        (implicit val context: ActorRefFactory) extends ApiRoute {
  type HIS = BifrostHistory
  type MS = BifrostState
  type VL = BWallet
  type MP = BifrostMemPool

  val SeedSize = 32

  override val route: Route = pathPrefix("utils") {
    utilsRoute
  }

  def utilsRoute: Route = path("") {
    entity(as[String]) { body =>
      withAuth {
        postJsonRoute {
          var reqId = ""
          parse(body) match {
            case Left(failure) => ApiException(failure.getCause)
            case Right(request) =>
              val response: Try[Json] = Try {
                val id = (request \\ "id").head.asString.get
                reqId = id
                require((request \\ "jsonrpc").head.asString.get == "2.0")
                val params = (request \\ "params").head.asArray.get
                require(params.size <= 5, s"size of params is ${params.size}")

                (request \\ "method").head.asString.get match {
                  case "seed" => seedRoute(params.head, id)
                  case "seedOfLength" => seedOfLength(params.head, id)
                  case "hashBlake2b" => hashBlake2b(params.head, id)
                }
              }
              response match {
                case Success(resp) => BifrostSuccessResponse(resp, reqId)
                case Failure(e) => BifrostErrorResponse(e, 500, reqId, verbose = settings.settingsJSON.getOrElse("verboseAPI", false.asJson).asBoolean.get)
              }
          }
        }
      }
    }
  }

  private def seed(length: Int): Json = {
    val seed = new Array[Byte](length)
    new SecureRandom().nextBytes(seed) //seed mutated here!
    Map("seed" -> Base58.encode(seed)).asJson
  }

  //Generates random seed
  private def seedRoute(params: Json, id: String): Json = {
    seed(SeedSize)
  }

  //Generates random seed of specified length
  private def seedOfLength(params: Json, id: String): Json = {
    val length: Int = (params \\ "length").head.asNumber.get.toInt.get
    seed(length)
  }

  //Returns Blake2b hash of specified message
  private def hashBlake2b(params: Json, id: String): Json = {
    val message: String = (params \\ "message").head.asString.get
    Map("message" -> message,
        "hash" -> Base58.encode(FastCryptographicHash(message))).asJson
  }
}

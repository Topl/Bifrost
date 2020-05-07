package bifrost.api.http

import java.security.SecureRandom

import akka.actor.ActorRefFactory
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

case class UtilsApiRoute(override val settings: Settings)(implicit val context: ActorRefFactory) extends ApiRoute {
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
            case Left(failure) => ErrorResponse(failure.getCause, 400, reqId)
            case Right(request) =>
              val response: Try[Json] = Try {
                val id = (request \\ "id").head.asString.get
                reqId = id
                require((request \\ "jsonrpc").head.asString.get == "2.0")
                val params = (request \\ "params").head.asArray.get
                require(params.size <= 5, s"size of params is ${params.size}")

                (request \\ "method").head.asString.get match {
                  case "seed"         => seedRoute(params.head, id)
                  case "seedOfLength" => seedOfLength(params.head, id)
                  case "hashBlake2b"  => hashBlake2b(params.head, id)
                }
              }
              response match {
                case Success(resp) => SuccessResponse(resp, reqId)
                case Failure(e) =>
                  ErrorResponse(
                    e,
                    500,
                    reqId,
                    verbose = settings.settingsJSON
                      .getOrElse("verboseAPI", false.asJson)
                      .asBoolean
                      .get
                  )
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

  /**  #### Summary
    *    Generates random seed of 32 bytes
    * 
    * ---
    *  #### Params
    * 
    *  | Fields                  	| Data type 	| Required / Optional 	| Description                                                            	|
    *  |-------------------------	|-----------	|---------------------	|------------------------------------------------------------------------	|
    *  | --None specified--       |           	|                     	|                                                                         |
    *
    * @param params input parameters as specified above
    * @param id request identifier
    * @return
    */
  private def seedRoute(params: Json, id: String): Json = {
    seed(SeedSize)
  }

  /**  #### Summary
    *    Generates random seed of specified length
    * 
    * ---
    *  #### Params
    *  | Fields                  	| Data type 	| Required / Optional 	| Description                                                            	|
    *  |-------------------------	|-----------	|---------------------	|------------------------------------------------------------------------	|
    *  | length                   | Int        	| Required             	| The number of characters to return                                      |
    *
    * @param params input parameters as specified above
    * @param id request identifier
    * @return
    */
  private def seedOfLength(params: Json, id: String): Json = {
    val length: Int = (params \\ "length").head.asNumber.get.toInt.get
    seed(length)
  }

  /** 
    *  #### Summary
    *    Returns Blake2b hash of specified message
    * 
    * ---
    *  #### Params
    *  | Fields                  	| Data type 	| Required / Optional 	| Description                                                            	|
    *  |-------------------------	|-----------	|---------------------	|------------------------------------------------------------------------	|
    *  | message                  | String     	| Required             	| The message that will be hashed                                         |
    *
    * @param params input parameters as specified above
    * @param id request identifier
    * @return
    */
  private def hashBlake2b(params: Json, id: String): Json = {
    val message: String = (params \\ "message").head.asString.get
    Map(
      "message" -> message,
      "hash" -> Base58.encode(FastCryptographicHash(message))
    ).asJson
  }
}

package co.topl.http.api.routes

import java.security.SecureRandom

import akka.actor.ActorRefFactory
import akka.http.scaladsl.server.Route
import co.topl.crypto.FastCryptographicHash
import co.topl.http.api.ApiRoute
import co.topl.nodeView.history.History
import co.topl.nodeView.mempool.MemPool
import co.topl.nodeView.state.State
import co.topl.settings.RESTApiSettings
import co.topl.wallet.Wallet
import io.circe.Json
import io.circe.syntax._
import scorex.crypto.encode.Base58

import scala.concurrent.{ ExecutionContext, Future }

case class UtilsApiRoute(override val settings: RESTApiSettings)
                        (implicit val context: ActorRefFactory, ec: ExecutionContext) extends ApiRoute {
  type HIS = History
  type MS = State
  type VL = Wallet
  type MP = MemPool
  override val route: Route = pathPrefix("utils") { basicRoute(handlers) }

  def handlers(method: String, params: Vector[Json], id: String): Future[Json] =
    method match {
      case "seed"         => seedRoute(params.head, id)
      case "seedOfLength" => seedOfLength(params.head, id)
      case "hashBlake2b"  => hashBlake2b(params.head, id)
    }

  private def generateSeed (length: Int): String = {
    val seed = new Array[Byte](length)
    new SecureRandom().nextBytes(seed) //seed mutated here!
    Base58.encode(seed)
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
  private def seedRoute(params: Json, id: String): Future[Json] = {
    val seedSize = 32 // todo: JAA - read this from a more appropriate place. Bip39 spec or something?
    Future(Map("seed" -> generateSeed(seedSize)).asJson)
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
  private def seedOfLength(params: Json, id: String): Future[Json] = {
    val length: Int = (params \\ "length").head.asNumber.get.toInt.get
    Future(Map("seed" -> generateSeed(length)).asJson)
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
  private def hashBlake2b(params: Json, id: String): Future[Json] = {
    val message: String = (params \\ "message").head.asString.get
    Future(Map(
      "message" -> message,
      "hash" -> Base58.encode(FastCryptographicHash(message))
    ).asJson)
  }
}

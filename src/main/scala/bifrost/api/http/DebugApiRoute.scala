package bifrost.api.http

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import bifrost.history.BifrostHistory
import bifrost.mempool.BifrostMemPool
import bifrost.modifier.box.proposition.PublicKey25519Proposition
import bifrost.settings.Settings
import bifrost.state.BifrostState
import bifrost.wallet.BWallet
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax._
import scorex.crypto.encode.Base58

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

case class DebugApiRoute(override val settings: Settings, nodeViewHolderRef: ActorRef)
                        (implicit val context: ActorRefFactory) extends ApiRouteWithView {

  type HIS = BifrostHistory
  type MS = BifrostState
  type VL = BWallet
  type MP = BifrostMemPool
  override val route: Route = pathPrefix("debug") { debugRoute }

  def debugRoute: Route = path("") {
    entity(as[String]) { body =>
      withAuth {
        postJsonRoute {
          viewAsync().map { view =>
            var reqId = ""
            parse(body) match {
              case Left(failure) => ErrorResponse(failure.getCause, 400, reqId)
              case Right(request) =>
                val futureResponse: Try[Future[Json]] = Try {
                  val id = (request \\ "id").head.asString.get
                  reqId = id
                  require((request \\ "jsonrpc").head.asString.get == "2.0")
                  val params = (request \\ "params").head.asArray.get
                  require(params.size <= 5, s"size of params is ${params.size}")

                  (request \\ "method").head.asString.get match {
                    case "info"       => infoRoute(params.head, id)
                    case "delay"      => delay(params.head, id)
                    case "myBlocks"   => myBlocks(params.head, id)
                    case "generators" => generators(params.head, id)
                  }
                }
                futureResponse map { response =>
                  Await.result(response, timeout.duration)
                } match {
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
  }

  /**  #### Summary
    *    Retrieve the best block
    *
    *  #### Description
    *    Find information about the current state of the chain including height, score, bestBlockId, etc
    *
    * ---
    *  #### Params
    *  | Fields                 	| Data type 	| Required / Optional 	| Description                                                            	|
    *  |--------------------------|-------------|-----------------------|-------------------------------------------------------------------------|
    *  | --None specified--       |           	|                     	|                                                                         |
    *
    * @param params input parameters as specified above
    * @param id request identifier
    * @return
    */
  private def infoRoute(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      Map(
        "height" -> view.history.height.toString.asJson,
        "score" -> view.history.score.asJson,
        "bestBlockId" -> Base58.encode(view.history.bestBlockId).asJson,
        "bestBlock" -> view.history.bestBlock.json,
        "stateVersion" -> Base58.encode(view.state.version).asJson
      ).asJson
    }
  }

  /**  #### Summary
    *    Calculate the average delay over a number of blocks
    *  
    *  #### Description
    *    Find the average delay between blocks starting from a specified blockId and till a certain number of blocks forged on top of it
    *
    * ---
    *  #### Params
    *  | Fields                 	| Data type 	| Required / Optional 	| Description                                                            	|
    *  |-------------------------	|-----------	|---------------------	|------------------------------------------------------------------------	|
    *  | blockId                  | String    	| Required            	| Id of block from which to start average delay computation               |
    *  | numBlocks               	| Int       	| Required            	| Number of blocks back to consider when computing average delay          |
    *
    * @param params input parameters as specified above
    * @param id request identifier
    * @return
    */
  private def delay(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val encodedSignature: String = (params \\ "blockId").head.asString.get
      val count: Int = (params \\ "numBlocks").head.asNumber.get.toInt.get
      Map(
        "delay" -> Base58
          .decode(encodedSignature)
          .flatMap(id => view.history.averageDelay(id, count))
          .map(_.toString)
          .getOrElse("Undefined")
          .asJson
      ).asJson
    }
  }

  /**  #### Summary
    *    Find the number of blocks forged by public keys held by the node
    * 
    *  #### Type
    *    Local Only -- An unlocked keyfile must be accessible (in local storage) to fulfill this request
    *
    * ---
    *  #### Params
    *  | Fields                  	| Data type 	| Required / Optional 	| Description                                                            	|
    *  |-------------------------	|-----------	|---------------------	|------------------------------------------------------------------------	|
    *  | --None specified--       |           	|                     	|                                                                         |
    *
    * @param params input parameters as specified above
    * @param id request identifier
    * @return
    */
  private def myBlocks(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val pubkeys: Set[PublicKey25519Proposition] =
        view.vault.publicKeys.flatMap {
          case pkp: PublicKey25519Proposition => Some(pkp)
          case _                              => None
        }
      val count =
        view.history.count(b => pubkeys.contains(b.forgerBox.proposition))
      Map(
        "pubkeys" -> pubkeys.map(pk => Base58.encode(pk.pubKeyBytes)).asJson,
        "count" -> count.asJson
      ).asJson
    }
  }

  /**  #### Summary
    *    Find distribution of block generators from all public keys in the chain's history
    *
    * ---
    *  #### Params
    *  | Fields                    	| Data type 	| Required / Optional 	| Description                                                            	|
    *  |-------------------------	|-----------	|---------------------	|------------------------------------------------------------------------	|
    *  | --None specified--        |           	|                     	|                                                                         |
    *
    * @param params input parameters as specified above
    * @param id request identifier
    * @return
    */
  private def generators(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val map: Map[String, Int] = view.history
        .forgerDistribution()
        .map(d => Base58.encode(d._1.pubKeyBytes) -> d._2)
      map.asJson
    }
  }
}

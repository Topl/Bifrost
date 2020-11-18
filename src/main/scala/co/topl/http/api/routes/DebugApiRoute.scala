package co.topl.http.api.routes

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import co.topl.attestation.Address
import co.topl.attestation.AddressEncoder.NetworkPrefix
import co.topl.http.api.ApiRouteWithView
import co.topl.modifier.ModifierId
import co.topl.nodeView.history.History
import co.topl.nodeView.mempool.MemPool
import co.topl.nodeView.state.State
import co.topl.settings.{AppContext, RESTApiSettings}
import io.circe.Json
import io.circe.syntax._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

case class DebugApiRoute(settings: RESTApiSettings, appContext: AppContext, nodeViewHolderRef: ActorRef)
                        (implicit val context: ActorRefFactory) extends ApiRouteWithView {

  type HIS = History
  type MS = State
  type MP = MemPool
  override val route: Route = { basicRoute(handlers) }

  // Establish the expected network prefix for addresses
  implicit val networkPrefix: NetworkPrefix = appContext.networkType.netPrefix

  def handlers(method: String, params: Vector[Json], id: String): Future[Json] =
    method match {
      case "info" => infoRoute(params.head, id)
      case "delay" => delay(params.head, id)
      case "generators" => generators(params.head, id)
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
    viewAsync().map {view =>
      Map(
        "height" -> view.history.height.toString.asJson,
        "score" -> view.history.score.asJson,
        "bestBlockId" -> view.history.bestBlockId.toString.asJson,
        "bestBlock" -> view.history.bestBlock.asJson,
        "stateVersion" -> view.state.version.toString.asJson
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
      val encodedSignature: ModifierId = ModifierId((params \\ "blockId").head.asString.get)
      val count: Int = (params \\ "numBlocks").head.asNumber.get.toInt.get
      Map(
        "delay" -> view.history.averageDelay(encodedSignature, count)
          .map(_.toString)
          .getOrElse("Undefined")
          .asJson
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
      val map: Map[Address, Int] = view.history
        .forgerDistribution()
        .map(d => d._1.address -> d._2)
      map.asJson
    }
  }
}

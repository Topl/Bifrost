package co.topl.http.api.endpoints

import akka.actor.{ActorRef, ActorRefFactory}
import co.topl.attestation.Address
import co.topl.attestation.AddressEncoder.NetworkPrefix
import co.topl.http.api.ApiEndpointWithView
import co.topl.modifier.ModifierId
import co.topl.nodeView.history.History
import co.topl.nodeView.mempool.MemPool
import co.topl.nodeView.state.State
import co.topl.settings.{AppContext, RPCApiSettings}
import io.circe.Json
import io.circe.syntax._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

case class DebugApiEndpoint (settings: RPCApiSettings, appContext: AppContext, nodeViewHolderRef: ActorRef)
                            (implicit val context: ActorRefFactory) extends ApiEndpointWithView {

  type HIS = History
  type MS = State
  type MP = MemPool

  // Establish the expected network prefix for addresses
  implicit val networkPrefix: NetworkPrefix = appContext.networkType.netPrefix

  // partial function for identifying local method handlers exposed by the api
  val handlers: PartialFunction[(String, Vector[Json], String), Future[Json]] = {
    case ("delay", params, id)      => delay(params.head, id)
    case ("generators", params, id) => generators(params.head, id)
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

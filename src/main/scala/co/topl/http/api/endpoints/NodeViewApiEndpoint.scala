package co.topl.http.api.endpoints

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
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

case class NodeViewApiEndpoint (override val settings: RPCApiSettings, appContext: AppContext, nodeViewHolderRef: ActorRef)
                               (implicit val context: ActorRefFactory) extends ApiEndpointWithView {
  type HIS = History
  type MS = State
  type MP = MemPool

  // Establish the expected network prefix for addresses
  implicit val networkPrefix: NetworkPrefix = appContext.networkType.netPrefix

  // partial function for identifying local method handlers exposed by the api
  val handlers: PartialFunction[(String, Vector[Json], String), Future[Json]] = {
    case ("topl_head", params, id)                   => getBestBlock(params.head, id)
    case ("topl_mempool", params, id)                => mempool(params.head, id)
    case ("topl_transactionById", params, id)        => transactionById(params.head, id)
    case ("topl_blockById", params, id)              => blockById(params.head, id)
    case ("topl_transactionFromMempool", params, id) => transactionFromMempool(params.head, id)
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
  private def getBestBlock(params: Json, id: String): Future[Json] = {
    viewAsync().map {view =>
      Map(
        "height" -> view.history.height.toString.asJson,
        "score" -> view.history.score.asJson,
        "bestBlockId" -> view.history.bestBlockId.toString.asJson,
        "bestBlock" -> view.history.bestBlock.asJson
      ).asJson
    }
  }

  /**  #### Summary
    *    Get the first 100 transactions in the mempool (sorted by fee amount)
    *
    * ---
    *  #### Params
    * 
    *  | Fields                  	| Data type 	| Required / Optional 	| Description                                 |
    *  |-------------------------	|-----------	|---------------------	|--------------------------------------------	|
    *  | --None specified--       |           	|                     	|                                             |
    *
    * @param params input parameters as specified above
    * @param id request identifier
    * @return
    */
  private def mempool(params: Json, id: String): Future[Json] = {
    viewAsync().map {
      view => view.pool.take(100).asJson
    }
  }

  /**  #### Summary
    *    Lookup a transaction by its id
    *
    * ---
    *  #### Params
    * 
    *  | Fields                  	| Data type 	| Required / Optional 	| Description                                	|
    *  |-------------------------	|-----------	|---------------------	|-------------------------------------------	|
    *  | transactionId            | String    	| Required            	| Base58 encoded transaction hash             |
    *
    * @param params input parameters as specified above
    * @param id request identifier
    * @return
    */
  private def transactionById(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      // parse required arguments
      val transactionId: ModifierId = ModifierId((params \\ "transactionId").head.asString.get)

      val blockId = view.history.blockContainingTx(transactionId).get
      val blockNumber = view.history.storage.heightOf(blockId)
      val tx = view.history.storage
        .modifierById(blockId)
        .get
        .transactions
        .filter(_.id == transactionId)
        .head

      tx.asJson.deepMerge{
        Map("blockNumber" -> blockNumber.toString,
          "blockId" -> blockId.toString
        ).asJson
      }
    }
  }

  /**  #### Summary
    *    Lookup a transaction in the mempool by its id
    *
    * ---
    *  #### Params
    * 
    *  | Fields                  	| Data type 	| Required / Optional 	| Description                               	|
    *  |-------------------------	|-----------	|---------------------	|--------------------------------------------	|
    *  | transactionId            | String    	| Required            	| Base58 encoded transaction hash             |
    *
    * @param params input parameters as specified above
    * @param id request identifier
    * @return
    */
  private def transactionFromMempool(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val transactionId: ModifierId = ModifierId((params \\ "transactionId").head.asString.get)
      view.pool.modifierById(transactionId) match {
        case Some(tx) => tx.asJson
        case None     => throw new Exception("Unable to retrieve transaction")
      }
    }
  }

  /**  #### Summary
    *   Lookup a block by its id
    *
    * ---
    *  #### Params
    * 
    *  | Fields                  	| Data type 	| Required / Optional 	| Description                                	|
    *  |-------------------------	|-----------	|---------------------	|--------------------------------------------	|
    *  | blockId                  | String    	| Required            	| Base58 encoded transaction hash             |
    *
    * @param params input parameters as specified above
    * @param id request identifier
    * @return
    */
  private def blockById(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val blockId: ModifierId = ModifierId((params \\ "blockId").head.asString.get)
      view.history
        .modifierById(blockId)
        .get
        .asJson
        .asObject
        .get
        .add("blockNumber", view.history.storage.heightOf(blockId).asJson)
        .add("blockDifficulty", view.history.storage.difficultyOf(blockId).asJson)
        .asJson
    }
  }
}
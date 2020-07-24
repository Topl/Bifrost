package bifrost.http.api.routes

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import bifrost.history.History
import bifrost.http.api.ApiRouteWithView
import bifrost.mempool.MemPool
import bifrost.modifier.ModifierId
import bifrost.nodeView.CurrentView
import bifrost.settings.AppSettings
import bifrost.state.State
import bifrost.wallet.Wallet
import io.circe.Json
import io.circe.syntax._
import scorex.crypto.encode.Base58

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

case class NodeViewApiRoute(override val settings: AppSettings, nodeViewHolderRef: ActorRef)
                           (implicit val context: ActorRefFactory) extends ApiRouteWithView {
  type HIS = History
  type MS = State
  type VL = Wallet
  type MP = MemPool
  type CV = CurrentView[History, State, Wallet, MemPool]
  override val route: Route = pathPrefix("nodeView") { basicRoute(handlers) }

  def handlers(method: String, params: Vector[Json], id: String): Future[Json] =
    method match {
      case "mempool"                => mempool(params.head, id)
      case "transactionById"        => transactionById(params.head, id)
      case "blockById"              => blockById(params.head, id)
      case "transactionFromMempool" => transactionFromMempool(params.head, id)
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
      view => view.pool.take(100).map(_.json).asJson
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
      val transactionId: String = (params \\ "transactionId").head.asString.get
      Base58.decode(transactionId) match {
        case Success(id) =>
          val storage = view.history.storage
          val blockIdWithPrefix = storage.blockIdOf(id).get
          val blockId = ModifierId(blockIdWithPrefix.tail)
          val blockNumber = storage.heightOf(blockId)
          val tx = storage
            .modifierById(blockId)
            .get
            .txs
            .filter(_.id.hashBytes sameElements id)
            .head
          tx.json.asObject.get
            .add("blockNumber", blockNumber.asJson)
            .add("blockHash", blockId.toString.asJson)
            .asJson
        case Failure(e) ⇒ throw e
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
      val transactionId: String = (params \\ "transactionId").head.asString.get
      val tx = Base58.decode(transactionId) match {
        case Success(txId) => view.pool.getById(ModifierId(txId))
        case Failure(_) => throw new Error("Unable to parse the provided transaction id")
      }

      tx match {
        case Some(tx) => tx.json
        case None => throw new Error("Unable to retrieve transaction")
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
      val modifierId: String = (params \\ "blockId").head.asString.get
      Base58.decode(modifierId) match {
        case Success(id) =>
          val blockId = ModifierId(id)
          val blockNumber = view.history.storage.heightOf(blockId)
          val storage = view.history.storage
          view.history
            .modifierById(blockId)
            .get
            .json
            .asObject
            .get
            .add("blockNumber", blockNumber.asJson)
            .add("blockDifficulty", storage.difficultyOf(blockId).asJson)
            .asJson
        case Failure(e) ⇒ throw e
      }
    }
  }
}
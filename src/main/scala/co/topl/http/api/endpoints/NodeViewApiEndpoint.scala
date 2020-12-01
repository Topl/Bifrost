package co.topl.http.api.endpoints

import akka.actor.{ActorRef, ActorRefFactory}
import co.topl.attestation.AddressEncoder.NetworkPrefix
import co.topl.http.api.{ApiEndpointWithView, Namespace, ToplNamespace}
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.Transaction
import co.topl.nodeView.history.History
import co.topl.nodeView.mempool.MemPool
import co.topl.nodeView.state.State
import co.topl.settings.{AppContext, RPCApiSettings}
import io.circe.{DecodingFailure, Json}
import io.circe.syntax._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

case class NodeViewApiEndpoint(
 override val settings: RPCApiSettings,
 appContext:            AppContext,
 nodeViewHolderRef:     ActorRef
)(implicit val context: ActorRefFactory) extends ApiEndpointWithView {
  type HIS = History
  type MS = State
  type MP = MemPool

  // Establish the expected network prefix for addresses
  implicit val networkPrefix: NetworkPrefix = appContext.networkType.netPrefix

  // the namespace for the endpoints defined in handlers
  val namespace: Namespace = ToplNamespace

  // partial function for identifying local method handlers exposed by the api
  val handlers: PartialFunction[(String, Vector[Json], String), Future[Json]] = {
    case (method, params, id) if method == s"${namespace.name}_head"                   => getBestBlock(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_mempool"                => mempool(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_transactionById"        => transactionById(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_blockById"              => blockById(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_transactionFromMempool" => transactionFromMempool(params.head, id)
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
    viewAsync().map { view =>
      Map(
        "height"      -> view.history.height.toString.asJson,
        "score"       -> view.history.score.asJson,
        "bestBlockId" -> view.history.bestBlockId.toString.asJson,
        "bestBlock"   -> view.history.bestBlock.asJson
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
  private def mempool(params: Json, id: String): Future[Json] = viewAsync().map(_.pool.take(100).asJson)


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
      (for {
        transactionId <- (params \\ "transactionId").head.as[ModifierId]
      } yield view.history.transactionById(transactionId)) match {
        case Right(Some((tx, blockId, height))) =>
          tx.asJson.deepMerge {
            Map(
              "blockNumber" -> height.toString,
              "blockId"     -> blockId.toString
            ).asJson
          }

        case Right(None) => throw new Exception(s"Unable to find confirmed transaction")
        case Left(ex)    => throw ex
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
      (for {
        transactionId <- (params \\ "transactionId").head.as[ModifierId]
      } yield view.pool.modifierById(transactionId)) match {
        case Right(Some(tx)) => tx.asJson
        case Right(None)     => throw new Exception("Unable to retrieve transaction")
        case Left(ex)        => throw ex
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
      (for {
        blockId <- (params \\ "blockId").head.as[ModifierId]
      } yield view.history.modifierById(blockId)) match {
        case Right(Some(block)) => block.asJson
        case Right(None)        => throw new Exception("The requested block could not be found")
        case Left(ex)           => throw ex
      }
    }
  }
}

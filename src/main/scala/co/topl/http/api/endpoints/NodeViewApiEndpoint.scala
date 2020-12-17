package co.topl.http.api.endpoints

import akka.actor.{ActorRef, ActorRefFactory}
import co.topl.attestation.Address
import co.topl.attestation.AddressEncoder.NetworkPrefix
import co.topl.http.api.{ApiEndpointWithView, Namespace, ToplNamespace}
import co.topl.modifier.ModifierId
import co.topl.nodeView.history.History
import co.topl.nodeView.mempool.MemPool
import co.topl.nodeView.state.State
import co.topl.nodeView.state.box.{ArbitBox, Box, PolyBox, TokenBox, TokenValueHolder}
import co.topl.settings.{AppContext, RPCApiSettings}
import io.circe.Json
import io.circe.syntax._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

case class NodeViewApiEndpoint(
  override val settings: RPCApiSettings,
  appContext:            AppContext,
  nodeViewHolderRef:     ActorRef
)(implicit val context:  ActorRefFactory)
    extends ApiEndpointWithView {
  type HIS = History
  type MS = State
  type MP = MemPool

  // Establish the expected network prefix for addresses
  implicit val networkPrefix: NetworkPrefix = appContext.networkType.netPrefix

  // the namespace for the endpoints defined in handlers
  val namespace: Namespace = ToplNamespace

  // partial function for identifying local method handlers exposed by the api
  val handlers: PartialFunction[(String, Vector[Json], String), Future[Json]] = {
    case (method, params, id) if method == s"${namespace.name}_head"            => getBestBlock(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_balances"        => balances(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_transactionById" => transactionById(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_blockById"       => blockById(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_mempool"         => mempool(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_transactionFromMempool" =>
      transactionFromMempool(params.head, id)
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
  private def getBestBlock(params: Json, id: String): Future[Json] =
    viewAsync { view =>
      Map(
        "height"      -> view.history.height.toString.asJson,
        "score"       -> view.history.score.asJson,
        "bestBlockId" -> view.history.bestBlockId.toString.asJson,
        "bestBlock"   -> view.history.bestBlock.asJson
      ).asJson
    }

  /** #### Summary
    * Lookup balances
    *
    * #### Type
    * Remote -- Transaction must be used in conjunction with an external key manager service.
    *
    * #### Description
    * Check balances of specified keys.
    *
    * #### Notes
    *    - Requires the Token Box Registry to be active
    *      ---
    *      #### Params
    *      | Fields                  	| Data type 	| Required / Optional 	| Description                                                            	  |
    *      |-------------------------	|-----------	|---------------------	|------------------------------------------------------------------------	  |
    *      | publicKey               	| String[]   	| Required            	| Public key whose balances are to be retrieved                            	|
    *
    * @param params input parameters as specified above
    * @param id     request identifier
    * @return
    */
  private def balances(params: Json, id: String): Future[Json] =
    viewAsync { view =>
      // parse arguments from the request
      (for {
        addresses <- (params \\ "addresses").head.as[Seq[Address]]
      } yield {
        // ensure we have the state being asked about
        checkAddress(addresses, view)

        val boxes: Map[Address, Map[String, Seq[TokenBox[TokenValueHolder]]]] =
          addresses
            .map(k => {
              val orderedBoxes = view.state.getTokenBoxes(k) match {
                case Some(boxes) => boxes.groupBy[String](Box.identifier(_).typeString)
                case _           => Map[String, Seq[TokenBox[TokenValueHolder]]]()
              }
              k -> orderedBoxes
            })
            .toMap

        val balances: Map[Address, Map[String, Long]] =
          boxes.map { case (addr, assets) =>
            addr -> assets.map { case (boxType, boxes) =>
              (boxType, boxes.map(_.value.quantity).sum)
            }
          }

        boxes.map { case (addr, boxes) =>
          addr -> Map(
            "Balances" -> Map(
              "Polys"  -> balances(addr).getOrElse(PolyBox.typeString, 0L),
              "Arbits" -> balances(addr).getOrElse(ArbitBox.typeString, 0L)
            ).asJson,
            "Boxes" -> boxes.map(b => b._1 -> b._2.asJson).asJson
          )
        }.asJson
      }) match {
        case Right(json) => json
        case Left(ex)    => throw ex
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
  private def mempool(params: Json, id: String): Future[Json] = viewAsync(_.pool.take(100).asJson)

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
  private def transactionById(params: Json, id: String): Future[Json] =
    viewAsync { view =>
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
  private def transactionFromMempool(params: Json, id: String): Future[Json] =
    viewAsync { view =>
      (for {
        transactionId <- (params \\ "transactionId").head.as[ModifierId]
      } yield view.pool.modifierById(transactionId)) match {
        case Right(Some(tx)) => tx.asJson
        case Right(None)     => throw new Exception("Unable to retrieve transaction")
        case Left(ex)        => throw ex
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
  private def blockById(params: Json, id: String): Future[Json] =
    viewAsync { view =>
      (for {
        blockId <- (params \\ "blockId").head.as[ModifierId]
      } yield view.history.modifierById(blockId)) match {
        case Right(Some(block)) => block.asJson
        case Right(None)        => throw new Exception("The requested block could not be found")
        case Left(ex)           => throw ex
      }
    }
}

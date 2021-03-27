package co.topl.http.api.endpoints

import akka.actor.{ActorRef, ActorRefFactory}
import akka.pattern.ask
import co.topl.attestation.Address
import co.topl.consensus.KeyManager.ReceivableMessages._
import co.topl.http.api.{ApiEndpointWithView, DebugNamespace, Namespace}
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.network.message.SyncInfo
import co.topl.nodeView.NodeViewHolder.ReceivableMessages.{GetDataFromCurrentView, GetNodeViewChanges}
import co.topl.nodeView.history.{History, HistoryDebug, HistoryReader}
import co.topl.nodeView.mempool.MemPool
import co.topl.nodeView.state.State
import co.topl.settings.{AppContext, RPCApiSettings}
import co.topl.utils.NetworkType.NetworkPrefix
import io.circe.Json
import io.circe.syntax._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

case class DebugApiEndpoint(
  settings:             RPCApiSettings,
  appContext:           AppContext,
  nodeViewHolderRef:    ActorRef,
  keyManagerRef:        ActorRef
)(implicit val context: ActorRefFactory)
    extends ApiEndpointWithView {

  type HIS = History
  type MS = State
  type MP = MemPool

  // Establish the expected network prefix for addresses
  implicit val networkPrefix: NetworkPrefix = appContext.networkType.netPrefix

  // the namespace for the endpoints defined in handlers
  val namespace: Namespace = DebugNamespace

  // partial function for identifying local method handlers exposed by the api
  val handlers: PartialFunction[(String, Vector[Json], String), Future[Json]] = {
    case (method, params, id) if method == s"${namespace.name}_delay"      => delay(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_myBlocks"   => myBlocks(params.head, id)
    case (method, params, id) if method == s"${namespace.name}_generators" => generators(params.head, id)
  }

  /** #### Summary
    * Calculate the average delay over a number of blocks
    *
    * #### Description
    * Find the average delay between blocks starting from a specified blockId and till a certain number of blocks
    * forged on top of it
    *
    * #### Params
    * | Fields    | Data type | Required / Optional | Description                                                    |
    * |-----------|-----------|---------------------|----------------------------------------------------------------|
    * | blockId   | String    | Required            | Id of block from which to start average delay computation      |
    * | numBlocks | Number    | Required            | Number of blocks back to consider when computing average delay |
    *
    * @param params input parameters as specified above
    * @param id request identifier
    * @return
    */
  private def delay(params: Json, id: String): Future[Json] =
    asyncHistory { hr =>
      (for {
        blockId <- params.hcursor.get[ModifierId]("blockId")
        count   <- params.hcursor.get[Int]("numBlocks")
      } yield new HistoryDebug(hr).averageDelay(blockId, count)) match {
        case Right(Success(delay)) => Map("delay" -> s"$delay milliseconds").asJson
        case Right(Failure(ex))    => throw ex
        case Left(ex)              => throw ex
      }
    }

  /** #### Summary
    * Find the number of blocks forged by addresses held by the node
    *
    * #### Type
    * Local Only -- An unlocked keyfile must be accessible (in local storage) to fulfill this request
    *
    * #### Params
    * | Fields             | Data type | Required / Optional | Description |
    * |--------------------|-----------|---------------------|-------------|
    * | --None specified-- |           |                     |             |
    *
    * @param params input parameters as specified above
    * @param id request identifier
    * @return
    */
  private def myBlocks(params: Json, id: String): Future[Json] =
    (nodeViewHolderRef ? GetNodeViewChanges(history = true, state = false, mempool = false))
      .mapTo[HistoryReader[Block, _ <: SyncInfo]]
      .flatMap { hr =>
      (keyManagerRef ? ListKeys).mapTo[Set[Address]].map { myKeys =>
        val blockNum = new HistoryDebug(hr).count { b =>
          myKeys.map(_.evidence).contains(b.generatorBox.evidence)
        }

        Map(
          "pubkeys" -> myKeys.asJson,
          "count"   -> blockNum.asJson
        ).asJson
      }
    }

  /** #### Summary
    * Find distribution of block generators from all addresses in the chain's history
    *
    * #### Params
    * | Fields             | Data type | Required / Optional | Description |
    * |--------------------|-----------|---------------------|-------------|
    * | --None specified-- |           |                     |             |
    *
    * @param params input parameters as specified above
    * @param id request identifier
    * @return
    */
  private def generators(params: Json, id: String): Future[Json] =
    asyncHistory { hr =>
      new HistoryDebug(hr)
        .forgerDistribution()
        .map(d => d._1.address -> d._2)
        .asJson
    }
}

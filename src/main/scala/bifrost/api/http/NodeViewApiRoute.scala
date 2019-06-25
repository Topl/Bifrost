package bifrost.api.http

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import bifrost.history.BifrostHistory
import bifrost.mempool.BifrostMemPool
import bifrost.state.BifrostState
import bifrost.wallet.BWallet
import io.circe.Json
import io.circe.parser.parse
import bifrost.scorexMod.GenericNodeViewHolder.{CurrentView, GetCurrentView}
import scorex.crypto.encode.Base58

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}
import akka.pattern.ask
import io.circe.syntax._
import bifrost.settings.Settings

import scala.concurrent.duration._


case class NodeViewApiRoute(override val settings: Settings, nodeViewHolderRef: ActorRef)
                           (implicit val context: ActorRefFactory) extends ApiRouteWithView {
  type HIS = BifrostHistory
  type MS = BifrostState
  type VL = BWallet
  type MP = BifrostMemPool
  override val route: Route = pathPrefix("nodeView") { nodeViewRoute }

  def nodeViewRoute: Route = path("") { entity(as[String]) { body =>
    withAuth {
      postJsonRoute {
        viewAsync().map { view =>
          var reqId = ""
          parse(body) match {
            case Left(failure) => ApiException(failure.getCause)
            case Right(request) =>
              val futureResponse: Try[Future[Json]] = Try {
                val id = (request \\ "id").head.asString.get
                reqId = id
                require((request \\ "jsonrpc").head.asString.get == "2.0")
                val params = (request \\ "params").head.asArray.get
                require(params.size <= 5, s"size of params is ${params.size}")

                (request \\ "method").head.asString.get match {
                  case "mempool" => mempool(params.head, id)
                  case "transactionById" => transactionById(params.head, id)
                  case "blockById" => blockById(params.head, id)
                  case "transactionFromMempool" => transactionFromMempool(params.head, id)
                }
              }
              futureResponse map {
                response => Await.result(response, timeout.duration)
              }
              match {
                case Success(resp) => BifrostSuccessResponse(resp, reqId)
                case Failure(e) => BifrostErrorResponse(e, 500, reqId, verbose = settings.settingsJSON.getOrElse("verboseAPI", false.asJson).asBoolean.get)
              }
          }
        }
      }
    }
  }
  }

//  private val source: ConnectedPeer = null

  private def getHistory(): Try[HIS] = Try {
    Await.result((nodeViewHolderRef ? GetCurrentView).mapTo[CurrentView[_, _ <: HIS, _, _]].map(_.history), 5.seconds)
      .asInstanceOf[HIS]
  }

  private def getMempool(): Try[MP] = Try {
    Await.result((nodeViewHolderRef ? GetCurrentView).mapTo[CurrentView[_, _, _, _ <: MP]].map(_.pool), 5.seconds)
      .asInstanceOf[MP]
  }

  //First 100 unconfirmed transactions in mempool
  private def mempool(params:Json, id: String): Future[Json] = {
    viewAsync().map {
      view =>
        getMempool() match {
          case Success(pool: MP) => pool.take(100).map(_.json).asJson
            //Failure is caught by BifrostErrorResponse in the nodeViewRoute function when the Await does not receive a response
        }
    }
  }

  private def transactionById(params: Json, id: String): Future[Json] = {
    viewAsync().map {
      view =>
        val transactionId: String = (params \\ "transactionId").head.asString.get
        Base58.decode(transactionId) match {
          case Success(id) =>
            val storage = view.history.storage
            val blockIdWithPrefix = storage.blockIdOf(id).get
            val blockId = blockIdWithPrefix.tail
            val blockNumber = storage.heightOf(blockId)
            val tx = storage.modifierById(blockId).get.txs.filter(_.id sameElements id).head
            tx.json.asObject.get.add("blockNumber", blockNumber.asJson)
              .add("blockHash", Base58.encode(blockId).asJson).asJson

//            (nodeViewHolderRef ? GetLocalObjects(null, Transaction.ModifierTypeId, Seq(id)))
//            .mapTo[ResponseFromLocal[_ <: NodeViewModifier]]
//            .map(_.localObjects.headOption.map(_.json)).asJson
        }
    }
  }

  private def transactionFromMempool(params: Json, id:String): Future[Json] = {
    viewAsync().map {
      view =>
        val transactionId: String = (params \\ "transactionId").head.asString.get
        Base58.decode(transactionId) match {
          case Success(id) => getMempool() match {
            case Success(pool: MP) => pool.getById(id).get.json.asJson
          }
        }
    }
  }

  private def blockById(params: Json, id: String): Future[Json] = {
    viewAsync().map {
      view =>
        val modifierId: String = (params \\ "blockId").head.asString.get
        Base58.decode(modifierId) match {
          case Success(id) =>
            val blockNumber = view.history.storage.heightOf(id)
            val storage = view.history.storage
//            view.history.modifierById(id).get.json.asObject.get.add("blockNumber", blockNumber.asJson).asJson
            view.history.modifierById(id).get.json.asObject.get.add("blockNumber", blockNumber.asJson).add("blockDifficulty",
            storage.difficultyOf(id).asJson).asJson
            //Old implementation in REST route
//            (nodeViewHolderRef ? GetLocalObjects(source, 1: Byte, Seq(id)))
//              .mapTo[ResponseFromLocal[_ <: NodeViewModifier]]
//              .map(_.localObjects.headOption.map(_.json).map(j => {
//                modifierId =
//                  j.asObject.get.add("blockNumber", blockNumber.asJson)
//              }))
        }
    }
  }

  //TODO Open surface was written in the REST api route but the openSurface method remains unimplemented
//  @Path("/openSurface")
//  @ApiOperation(value = "Ids of open surface", notes = "Ids of open surface in history", httpMethod = "GET")
//  def openSurface: Route = path("openSurface") {
//    getJsonRoute {
//      getHistory() match {
//        case Success(history: HIS) => SuccessApiResponse(history
//          .openSurfaceIds()
//          .map(Base58.encode)
//          .asJson)
//        case Failure(e) => ApiException(e)
//      }
//    }
//  }


}

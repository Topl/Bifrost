package bifrost.scorexMod.api.http

import javax.ws.rs.Path

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import bifrost.api.http.ApiRouteWithView
import io.circe.syntax._
import io.swagger.annotations._
import bifrost.scorexMod.GenericNodeViewHolder.{CurrentView, GetCurrentView}
import scorex.core.api.http.ApiRoute
import scorex.core.api.http._
import scorex.core.consensus.History
import scorex.core.network.ConnectedPeer
import bifrost.scorexMod.GenericNodeViewSynchronizer.{GetLocalObjects, ResponseFromLocal}
import scorex.core.settings.Settings
import scorex.core.transaction.{MemoryPool, Transaction}
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.{NodeViewModifier, PersistentNodeViewModifier}
import scorex.crypto.encode.Base58

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}


@Path("/nodeView")
@Api(value = "/nodeView", produces = "application/json")
case class GenericNodeViewApiRoute[P <: Proposition, TX <: Transaction[P]]
(override val settings: Settings, nodeViewHolderRef: ActorRef)
(implicit val context: ActorRefFactory) extends ApiRouteWithView {

  override val route = pathPrefix("nodeView") {
    openSurface ~ persistentModifierById ~ pool ~ transactionById
  }

  type PM <: PersistentNodeViewModifier[P, TX]
  type HIS <: History[P, TX, PM, _, _ <: History[P, TX, PM, _, _]]
  type MP <: MemoryPool[TX, _ <: MemoryPool[TX, _]]

  //TODO null?
  private val source: ConnectedPeer = null

  def getHistory(): Try[HIS] = Try {
    Await.result((nodeViewHolderRef ? GetCurrentView).mapTo[CurrentView[_, _ <: HIS, _, _]].map(_.history), 5.seconds)
      .asInstanceOf[HIS]
  }

  def getMempool(): Try[MP] = Try {
    Await.result((nodeViewHolderRef ? GetCurrentView).mapTo[CurrentView[_, _, _, _ <: MP]].map(_.pool), 5.seconds)
      .asInstanceOf[MP]
  }

  @Path("/pool")
  @ApiOperation(value = "Pool", notes = "Pool of unconfirmed transactions", httpMethod = "GET")
  def pool: Route = path("pool") {
    getJsonRoute {
      getMempool() match {
        case Success(pool: MP) => SuccessApiResponse(pool.take(1000).map(_.json).asJson)
        case Failure(e) => ApiException(e)
      }
    }
  }

  @Path("/openSurface")
  @ApiOperation(value = "Ids of open surface", notes = "Ids of open surface in history", httpMethod = "GET")
  def openSurface: Route = path("openSurface") {
    getJsonRoute {
      getHistory() match {
        case Success(history: HIS) => SuccessApiResponse(history.openSurfaceIds().map(Base58.encode).asJson)
        case Failure(e) => ApiException(e)
      }
    }
  }


  @Path("/persistentModifier/{id}")
  @ApiOperation(value = "Persistent modifier by id", notes = "Persistent modifier by id", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "id", value = "block id ", required = true, dataType = "string", paramType = "path")
  ))
  def persistentModifierById: Route = path("persistentModifier" / Segment) { case encodedId =>
    getJsonRoute {
      viewAsync().flatMap{ view =>
        Base58.decode(encodedId) match {
          case Success(id) =>
            val blockNumber = view.history.storage.heightOf(id)
            //TODO 1: Byte
            (nodeViewHolderRef ? GetLocalObjects(source, 1: Byte, Seq(id)))
              .mapTo[ResponseFromLocal[_ <: NodeViewModifier]]
              .map(_.localObjects.headOption.map(_.json).map(j => {
                val modifiedJson = j.asObject.get.add("blockNumber", blockNumber.asJson).asJson
                SuccessApiResponse(modifiedJson)
              })
                .getOrElse(ApiError.blockNotExists))
          case _ => Future(ApiError.blockNotExists)
        }
      }
    }
  }

  @Path("/transaction/{id}")
  @ApiOperation(value = "Transaction by id", notes = "Transaction by id", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "id", value = "Transaction id ", required = true, dataType = "string", paramType = "path")
  ))
  def transactionById: Route = path("transaction" / Segment) { case encodedId =>
    getJsonRoute {
      viewAsync().flatMap { view =>
        Base58.decode(encodedId) match {
          case Success(id) =>
            val storage = view.history.storage
            val blockIdWithPrefix = storage.blockIdOf(id).get
            require(blockIdWithPrefix.head == Transaction.ModifierTypeId)
            val blockId = blockIdWithPrefix.tail
            val blockNumber = storage.heightOf(blockId)
            val tx = storage.modifierById(blockId).get.txs.filter(_.id sameElements id).head

            val modifiedJson = tx.json.asObject.get.add("blockNumber", blockNumber.asJson)
            .add("blockHash", Base58.encode(blockId).asJson).asJson
            Future(SuccessApiResponse(data = modifiedJson))
          case _ => Future(ApiError.transactionNotExists)
        }
      }
    }
  }
}

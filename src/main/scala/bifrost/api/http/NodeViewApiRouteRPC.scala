package bifrost.api.http

//WalletRPCRoute imports
import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import bifrost.history.BifrostHistory
import bifrost.mempool.BifrostMemPool
import bifrost.state.BifrostState
import bifrost.transaction.PolyTransfer
import bifrost.transaction.box.{ArbitBox, PolyBox}
import bifrost.wallet.BWallet
import io.circe.Json
import io.circe.parser.parse
import io.circe.parser._
import io.circe.syntax._
import scorex.core.LocalInterface.LocallyGeneratedTransaction
import scorex.core.NodeViewHolder.{CurrentView, GetCurrentView}
import scorex.core.api.http.{ApiException, SuccessApiResponse}
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.{ProofOfKnowledgeProposition, PublicKey25519Proposition}
import scorex.core.transaction.state.PrivateKey25519
import scorex.crypto.encode.Base58

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}



//NodeViewApiRoute imports
import javax.ws.rs.Path

import akka.pattern.ask
import io.circe.syntax._
import io.swagger.annotations._
import scorex.core.consensus.History
import scorex.core.network.ConnectedPeer
import scorex.core.network.NodeViewSynchronizer.{GetLocalObjects, ResponseFromLocal}
import scorex.core.settings.Settings
import scorex.core.transaction.{MemoryPool, Transaction}
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.{NodeViewModifier, PersistentNodeViewModifier}

import scala.concurrent.duration._



case class NodeViewApiRouteRPC(override val settings: Settings, nodeViewHolderRef: ActorRef)
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
            case Right(json) =>
              val futureResponse: Try[Future[Json]] = Try {
                val id = (json \\ "id").head.asString.get
                reqId = id
                require((json \\ "jsonrpc").head.asString.get == "2.0")
                val params = (json \\ "params").head.asArray.get
                require(params.size <= 5, s"size of params is ${params.size}")

                (json \\ "method").head.asString.get match {
                  case "pool" => pool(params.head, id)
                  case "transactionById" => transactionById(params.head, id)
                  case "persistentModifierById" => persistentModifierById(params.head, id)
                }
              }
              futureResponse map {
                response => Await.result(response, timeout.duration)
              }
              match {
                case Success(resp) => BifrostSuccessResponse(resp, reqId)
                case Failure(e) => BifrostErrorResponse(e, 500, reqId, verbose = settings.settingsJSON.getOrElse("verboseAPI", flase.asJson).asBoolean.get)
              }
          }

        }
      }
    }
  }
  }


  private def getHistory(): Try[HIS] = Try {
    Await.result((nodeViewHolderRef ? GetCurrentView).mapTo[CurrentView[_, _ <: HIS, _, _]].map(_.history), 5.seconds)
      .asInstanceOf[HIS]
  }

  private def getMempool(): Try[MP] = Try {
    Await.result((nodeViewHolderRef ? GetCurrentView).mapTo[CurrentView[_, _, _, _ <: MP]].map(_.pool), 5.seconds)
      .asInstanceOf[MP]
  }

  //First 1000 unconfirmed transactions in memPool
  private def pool(params:Json, id: String): Future[Json] = {
    viewAsync().map {
      view =>
        getMempool() match {
          case Success(pool: MP) => pool.take(1000).map(_.json).asJson
            //Failure is caught by BifrostErrorResponse in the nodeViewRoute function when the Await does not receive a response

        }
    }
  }

  //openSurface function is unimplemented in History

  private def transactionById(params: Json, id: String): Future[Json] = {
    viewAsync().map {
      view =>
        val transactionId: String = (params \\ "transactionId").head.asString.get
        Base58.decode(transactionId) match {
          case Success(id) =>
            (nodeViewHolderRef ? GetLocalObjects(null, Transaction.ModifierTypeId, Seq(id)))
            .mapTo[ResponseFromLocal[_ <: NodeViewModifier]]
            .map(_.localObjects.headOption.map(_.json)).asJson
          }

//        (nodeViewHolderRef ? GetLocalObjects(null, Transaction.ModifierTypeId, Seq(Base58.decode(transactionId))))
//          .mapTo[ResponseFromLocal[_ <: NodeViewModifier]]
//          .map(_.localObjects.headOption.map(_.json).asJson)
    }
  }

  private def persistentModifierById(params: Json, id: String): Future[Json] = {
    viewAsync().map {
      view =>

    }
  }


}

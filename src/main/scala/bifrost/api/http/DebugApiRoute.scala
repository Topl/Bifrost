package bifrost.api.http

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import bifrost.history.BifrostHistory
import bifrost.mempool.BifrostMemPool
import bifrost.state.BifrostState
import bifrost.wallet.BWallet
import bifrost.settings.Settings
import io.circe.Json
import io.circe.syntax._
import io.circe.parser.parse
import bifrost.consensus.History.HistoryComparisonResult
import bifrost.transaction.box.proposition.PublicKey25519Proposition
import scorex.crypto.encode.Base58

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

case class DebugApiRoute(override val settings: Settings, nodeViewHolderRef: ActorRef)
                        (implicit val context: ActorRefFactory) extends ApiRouteWithView {

  type HIS = BifrostHistory
  type MS = BifrostState
  type VL = BWallet
  type MP = BifrostMemPool
  override val route: Route = pathPrefix("debug") { debugRoute }

  //noinspection ScalaStyle
  def debugRoute: Route = path("") { entity(as[String]) { body =>
    withAuth {
      postJsonRoute {
        viewAsync().map {
          view =>
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
                    case "info" => infoRoute(params.head, id)
                    case "delay" => delay(params.head, id)
                    case "myBlocks" => myBlocks(params.head, id)
                    case "generators" => generators(params.head, id)
                    case "chain" => chain(params.head, id)
                    //                    case "sync" => sync(params.head, id)
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

  private def infoRoute(params: Json, id: String): Future[Json] = {
      viewAsync().map {
        view =>
            Map(
              "height" -> view.history.height.toString.asJson,
              "score" -> view.history.score.asJson,
              "bestBlockId" -> Base58.encode(view.history.bestBlockId).asJson,
              "bestBlock" -> view.history.bestBlock.json,
              "stateVersion" -> Base58.encode(view.state.version).asJson
            ).asJson
      }
    }

  private def delay(params: Json, id: String): Future[Json] = {
    viewAsync().map {
      view =>
        val encodedSignature: String = (params \\ "blockId").head.asString.get
        val count: Int = (params \\ "numBlocks").head.asNumber.get.toInt.get
        Map(
          "delay" -> Base58.decode(encodedSignature).flatMap(id => view.history.averageDelay(id, count))
            .map(_.toString).getOrElse("Undefined").asJson
        ).asJson
    }
  }

  private def myBlocks(params: Json, id: String): Future[Json] = {
    viewAsync().map {
      view =>
        val pubkeys: Set[PublicKey25519Proposition] = view.vault.publicKeys.flatMap {
          case pkp: PublicKey25519Proposition => Some(pkp)
          case _ => None
        }
        val count = view.history.count(b => pubkeys.contains(b.forgerBox.proposition))
        Map(
          "pubkeys" -> pubkeys.map(pk => Base58.encode(pk.pubKeyBytes)).asJson,
          "count" -> count.asJson
        ).asJson
    }
  }

  private def generators(params: Json, id: String): Future[Json] = {
    viewAsync().map {
      view =>
        val map: Map[String, Int] = view.history.forgerDistribution()
          .map(d => Base58.encode(d._1.pubKeyBytes) -> d._2)
        map.asJson
    }
  }

  private def chain(params: Json, id: String): Future[Json] = {
    viewAsync().map {
      view =>
        Map(
          "history" -> view.history.toString
        ).asJson
    }
  }

  //TODO unimplemented
//  private def sync(params: Json, id: String): Future[Json] = {
//    viewAsync.map {
//      view =>
//        if(view.history.syncInfo(false) == HistoryComparisonResult.Equal)
//          Map("synced" -> "True").asJson
//        else
//          Map("synced" -> "False").asJson
//    }
//  }

}

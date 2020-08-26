package http

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model._
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Sink
import akka.util.ByteString
import requests.Requests
import io.circe.Json

import scala.concurrent.Future

class GjallahornApiRoute {

  implicit val actorsystem = ActorSystem()
  implicit val materializer: ActorMaterializer = ActorMaterializer()

  val serverSource = Http().newServerAt("localhost", 9086).connectionSource()

  val r = new Requests

//  def handlers(method: String, params: Vector[Json], id: String): Future[Json] =
//    method match {
//      case "transaction" => r.transaction(params.head, id)
//      case "signTx" => r.signTx(params.head, id)
//      case "broadcastTx" => r.broadcastTx(params.head, id)
//    }

  val requestHandler: HttpRequest => HttpResponse = {
    case HttpRequest(POST, Uri.Path("/transaction"), _, entity, _) =>
      val postBody = r.byteStringToJSON(entity.dataBytes.runFold(ByteString.empty) { case (acc, b) => acc ++ b })
      val tx = r.transaction((postBody \\ "method"), )
      HttpResponse(StatusCodes.OK, entity = HttpEntity(ContentTypes.`application/json`,))

    case HttpRequest(GET, Uri.Path("/ping"), _, _, _) =>
      HttpResponse(entity = "PONG!")

    case HttpRequest(GET, Uri.Path("/crash"), _, _, _) =>
      sys.error("BOOM!")

    case r: HttpRequest =>
      r.discardEntityBytes() // important to drain incoming HTTP Entity stream
      HttpResponse(404, entity = "Unknown resource!")
  }

}

/*
  transaction

  broadcast

  sign

  postJSON route

  API response.scala

  basicRoute --> check dev branch - in ApiRoute
}
routes

need a handler (look at AssetAPI Route)
 */
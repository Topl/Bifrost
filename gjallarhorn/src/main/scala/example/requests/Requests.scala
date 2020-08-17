package example.requests

//Import relevant actor libraries
import akka.actor.Status.{Failure, Success}
import akka.actor.{Actor, ActorLogging, ActorSystem}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.RawHeader
import akka.stream.ActorMaterializer
import akka.util.{ByteString, Timeout}
import example.PublicKey25519Proposition

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.concurrent.duration._
import scala.util.Try

class Requests extends { //Actor with ActorLogging {

  /*
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()
  implicit val executionContext: ExecutionContextExecutor = system.dispatcher
  val http = Http(system)

   */

  val timeout: Timeout = Timeout(10.seconds)

  //override def receive: Receive = ???

  //Generic Method for HTTP POST request
  def httpPOST(jsonRequest: ByteString): HttpRequest = {
    HttpRequest(
      HttpMethods.POST,
      uri = "http://localhost:9085/asset/",
      entity = HttpEntity(MediaTypes.`application/json`, jsonRequest)
    ).withHeaders(RawHeader("x-api-key", "test_key"))
  }

  //Generic fields for transaction
  def transaction(method: String, issuer: String, recipient: String, amount: Int): ByteString = {
    var requestBody: ByteString = ByteString.empty
    method match {
      case "createAssetsPrototype" => {
        requestBody = ByteString(
          s"""
             |{
             |   "jsonrpc": "2.0",
             |   "id": "2",
             |   "method": "$method",
             |   "params": [{
             |     "issuer": "$issuer",
             |     "recipient": "$recipient",
             |     "amount": $amount,
             |     "assetCode": "etherAssets",
             |     "fee": 0,
             |     "data": ""
             |   }]
             |}
         """.stripMargin)
      }
      case _ =>
    }

    requestBody
  }

  /*def sendRequest(request: ByteString): Future[String]  = {

    var res: String = ""

    Http().singleRequest(httpPOST(request)).map {
      case response@HttpResponse(StatusCodes.OK, headers, entity, _) =>
        entity.dataBytes.runFold(ByteString(""))(_ ++ _).foreach { body =>
          res = body.utf8String
        }
        res
      case _ => sys.error("something wrong")
    }
  }
}

   */
}


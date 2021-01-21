package requests

import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.{Http, HttpExt}
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.RawHeader
import akka.pattern.ask
import akka.util.{ByteString, Timeout}
import attestation.Address
import io.circe.parser.parse
import io.circe.{Json, parser}
import io.circe.syntax._
import keymanager.KeyManager.SignTx
import requests.RequestsManager.BifrostRequest

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import settings.ApplicationSettings


class Requests (settings: ApplicationSettings, keyManagerRef: ActorRef)
               (implicit val actorSystem: ActorSystem) {

  val http: HttpExt = Http(actorSystem)
  private var requestsManager: Option[ActorRef] = None

  implicit val timeout: Timeout = Timeout(10.seconds)

  val declaredAddress: String = settings.declaredAddress

  //TODO: does the http require a path anymore on Bifrost side?
  //Generic Method for HTTP POST request
  def httpPOST(jsonRequest: ByteString, path: String = ""): HttpRequest = {
    HttpRequest(
      HttpMethods.POST,
      uri = s"$declaredAddress/$path/",
      entity = HttpEntity(MediaTypes.`application/json`, jsonRequest)
    ).withHeaders(RawHeader("x-api-key", "test_key"))
  }

  def requestResponseByteString(request: HttpRequest): Future[ByteString] = {
    val response = http.singleRequest(request)
    response.flatMap {
      case _@HttpResponse(StatusCodes.OK, _, entity, _) =>
        entity.dataBytes.runFold(ByteString.empty) { case (acc, b) => acc ++ b }
      case _ => sys.error("something wrong")
    }
  }

  def futureByteStringToJSON(data: Future[ByteString]): Json = {
    val parsedData: Future[Json] = data.map { x => byteStringToJSON(x)}
    Await.result(parsedData, 20 seconds)
  }

  def byteStringToJSON(data: ByteString): Json = {
    parser.parse(data.utf8String) match {
        case Right(parsed) => parsed
        case Left(e) => throw e.getCause
      }
  }

  def createJsonResponse (transaction: Json, result: Json): Json = {
    val resultString = result.toString().replace("\\", "").replace("\"{", "{")
      .replace("}\"", "}")
    var resultJson: Json = result
    parse(resultString) match {
      case Left(f) => throw f
      case Right(res: Json) => resultJson = res
    }
    Map(
      "jsonrpc" -> (transaction \\ "jsonrpc").head.asJson,
      "id" -> (transaction \\ "id").head.asJson,
      "result" -> resultJson
    ).asJson
  }


  def signTx(transaction: Json, signingKeys: IndexedSeq[Address]): Json = {
    val result = (transaction \\ "result").head
    val tx = (result \\ "rawTx").head
    val messageToSign = (result \\ "messageToSign").head.asString.get
    val newResult = Await.result((keyManagerRef ? SignTx(tx, signingKeys, messageToSign)).mapTo[Json], 10.seconds)
    createJsonResponse(transaction, newResult)
  }


  def transaction(method: String, innerParams: Json): ByteString = {
    var requestBody: ByteString = ByteString.empty
    requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "2",
           |   "method": "$method",
           |   "params": [$innerParams]
           |}
       """.stripMargin)

    requestBody
  }

  /**
    *
    * @param request - the request to send as a byteString
    * @return
    */
  def sendRequest(request: ByteString): Json  = {
    requestsManager match {
      case Some(actor) =>
        settings.communicationMode match {
          case "useTcp" =>
            val sendTx = httpPOST(request)
            val data = requestResponseByteString(sendTx)
            futureByteStringToJSON(data)

          case "useAkka" =>
            val req: Json = byteStringToJSON(request)
            val result = Await.result((actor ? BifrostRequest(req)).mapTo[String].map(_.asJson), 10.seconds)
            createJsonResponse(req, result)
        }
      case None =>
        val msg = "cannot send request because you are offline mode " +
          "or the chain provider provided was incorrect."
        throw new Exception(msg)
    }

  }

  def broadcastTx(signedTransaction: Json): Json = {
    sendRequest(transaction("topl_broadcastTx", signedTransaction))
  }

  def getBalances (publicKeys: Set[String]): Json = {
    val keysJson: Set[Json] = publicKeys.map(_.asJson)
    val params: Json = Map("addresses" -> keysJson.toList).asJson
    val requestBody = transaction("topl_balances", params)
    sendRequest(requestBody)
  }

  def switchOnlineStatus(requestsManagerRef: Option[ActorRef]): Unit = {
    requestsManager = requestsManagerRef
  }

}



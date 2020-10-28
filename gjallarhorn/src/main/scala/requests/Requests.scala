package requests

import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.{Http, HttpExt}
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.RawHeader
import akka.pattern.ask
import akka.util.{ByteString, Timeout}
import keymanager.Keys
import crypto._
import io.circe.parser.parse
import io.circe.{Json, parser}
import io.circe.syntax._
import requests.RequestsManager.{AssetRequest, WalletRequest}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scorex.util.encode.Base58
import settings.AppSettings



class Requests (settings: AppSettings, requestsManager: ActorRef)
               (implicit val actorSystem: ActorSystem) {

  val http: HttpExt = Http(actorSystem)

  implicit val timeout: Timeout = Timeout(10.seconds)

  val declaredAddress = settings.declaredAddress

  //Generic Method for HTTP POST request
  def httpPOST(jsonRequest: ByteString, path: String): HttpRequest = {
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
        println("http response: " + response)
        entity.dataBytes.runFold(ByteString.empty) { case (acc, b) => acc ++ b }
      case _ => sys.error("something wrong")
    }
  }

  def byteStringToJSON(data: Future[ByteString]): Json = {
    val parsedData: Future[Json] = data.map { x =>
      println("data: " + x.utf8String)
      parser.parse(x.utf8String) match {
        case Right(parsed) => parsed
        case Left(e) => throw e.getCause
      }
    }
    Await.result(parsedData, 20 seconds)
  }

  def byteStringToJSON(data: ByteString): Json = {
    parser.parse(data.utf8String) match {
        case Right(parsed) => parsed
        case Left(e) => throw e.getCause
      }
  }

  def jsonToByteString(data: Json): ByteString = {
    val result = (data \\ "result").head
    val tx = (result \\ "formattedTx").head
    val params = Map(
      "tx" -> tx
    ).asJson
    val newJSON = Map(
      "jsonrpc" -> (data \\ "jsonrpc").head,
      "id" -> (data \\ "id").head,
      "method" -> "broadcastTx".asJson,
      "params" -> List(params).asJson
    ).asJson
    ByteString(newJSON.toString.getBytes())
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


  def signTx(transaction: Json, keyManager: Keys, signingKeys: List[String]): Json = {
    val result = (transaction \\ "result").head
    val tx = (result \\ "formattedTx").head
    val messageToSign = (result \\ "messageToSign").head
    assert(signingKeys.contains((tx \\ "issuer").head.asString.get))

    val sigs: List[(String, String)] = signingKeys.map { pk =>
      val pubKey = PublicKey25519Proposition(pk)

      val privKey = keyManager.secrets.find(sk => sk.publicKeyBytes sameElements pubKey.pubKeyBytes)

      privKey match {
        case Some(sk) => {
            val signature = Base58.encode(sk.sign(Base58.decode(messageToSign.asString.get).get).signature)
            (pk, signature)
        }
        case None => throw new NoSuchElementException
      }
    }

    val newTx = tx.deepMerge(Map(
      "signatures" -> sigs.toMap.asJson
    ).asJson)
    val newResult = Map("formattedTx"-> newTx).asJson
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
    * @param request
    * @param path - asset or wallet request
    * @return
    */
  def sendRequest(request: ByteString, path: String): Json  = {
    settings.useApiRoute match {
      case true =>
        val sendTx = httpPOST(request, path)
        val data = requestResponseByteString(sendTx)
        byteStringToJSON(data)

      case false =>
        val req: Json = byteStringToJSON(request)
        path match {
          case "asset" =>
            val result = Await.result((requestsManager ? AssetRequest(req)).mapTo[String].map(_.asJson), 10.seconds)
            createJsonResponse(req, result)
          case "wallet" =>
            val result = Await.result(
              (requestsManager ? WalletRequest(req)).mapTo[String].map(_.asJson), 10.seconds)
            createJsonResponse(req, result)
        }
    }
  }

  def broadcastTx(signedTransaction: Json): Json = {
    sendRequest(jsonToByteString(signedTransaction), "wallet")
  }

  def getBalances (publicKeys: Set[String]): Json = {
    val keysWithQuotes: Set[String] = publicKeys.map(pk => s""""$pk"""")
    val keys: String = keysWithQuotes.mkString(", \n")
    val json = (
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "1",
         |   "method": "balances",
         |   "params": [
         |      {
         |        "publicKeys": [
         |            $keys
         |        ]
         |      }
         |   ]
         |}
       """
    )
    val requestBody = ByteString(json.stripMargin)
    sendRequest(requestBody, "wallet")
  }
}



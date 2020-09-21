package requests

import akka.actor.ActorSystem
import akka.http.scaladsl.{Http, HttpExt}
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.RawHeader
import akka.stream.ActorMaterializer
import akka.util.{ByteString, Timeout}
import keymanager.Keys
import crypto.{PrivateKey25519Companion, PublicKey25519Proposition, _}
import io.circe.{Json, parser}
import io.circe.syntax._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scorex.crypto.encode.Base58

import scala.util.Try

class Requests extends {
  implicit val actorSystem: ActorSystem = ActorSystem()
  implicit val materializer: ActorMaterializer = ActorMaterializer()

  val http: HttpExt = Http(actorSystem)

  val timeout: Timeout = Timeout(10.seconds)

  //Generic Method for HTTP POST request
  def httpPOST(jsonRequest: ByteString, path: String): HttpRequest = {
    HttpRequest(
      HttpMethods.POST,
      uri = s"http://localhost:9085/$path/",
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

  def byteStringToJSON(data: Future[ByteString]): Json = {
    val parsedData: Future[Json] = data.map { x =>
      parser.parse(x.utf8String) match {
        case Right(parsed) => parsed
        case Left(e) => throw e.getCause
      }
    }
    Await.result(parsedData, 5 seconds)
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

  def signTx(transaction: Json, keyManager: Keys, signingKeys: List[String]): Json = {
    val result = (transaction \\ "result").head
    val tx = (result \\ "formattedTx").head
    val messageToSign = (result \\ "messageToSign").head
    assert(signingKeys.contains((tx \\ "issuer").head.asString.get))

    var sigs: List[(String, String)] = signingKeys.map { pk =>
      val pubKey = PublicKey25519Proposition(Base58.decode(pk).get)
      val privKey = keyManager.secrets.find(sk => sk.publicKeyBytes sameElements pubKey.pubKeyBytes)

      privKey match {
        case Some(sk) => {
          val signature = Base58.encode(PrivateKey25519Companion.sign(sk, Base58.decode(messageToSign.asString.get).get).signature)
          (pk, signature)
        }
        case None => throw new NoSuchElementException
      }
    }

    val newTx = tx.deepMerge(Map(
      "signatures" -> sigs.toMap.asJson
    ).asJson)
    val newResult = Map("formattedTx"-> newTx).asJson
    Map(
      "jsonrpc" -> (transaction \\ "jsonrpc").head.asJson,
      "id" -> (transaction \\ "id").head.asJson,
      "result" -> newResult
    ).asJson
  }

  def transaction(method: String, issuer: String, recipient: String, amount: Long): ByteString = {
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

  def sendRequest(request: ByteString, path: String): Json  = {
    val sendTx = httpPOST(request, path)
    val data = requestResponseByteString(sendTx)
    byteStringToJSON(data)
  }

  def broadcastTx(signedTransaction: Json): Json = {
    val tx = jsonToByteString(signedTransaction)
    sendRequest(tx, "wallet")
  }

}


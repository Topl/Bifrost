package example.requests

//Import relevant actor libraries
import akka.actor.Status.{Failure, Success}
import akka.actor.{Actor, ActorLogging, ActorSystem}
import akka.http.scaladsl.{Http, HttpExt}
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.RawHeader
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Source
import akka.util.{ByteString, Timeout}
import example.KeyManager.getListOfFiles
import example.{PrivateKey25519Companion, PublicKey25519Proposition, _}
import io.circe.{Json, parser}
import io.circe.syntax._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor, Future}
import scala.concurrent.duration._
import scala.util.Try
import scorex.crypto.encode.Base58

class Requests extends { //Actor with ActorLogging {
  implicit val actorSystem: ActorSystem = ActorSystem()
  implicit val materializer: ActorMaterializer = ActorMaterializer()

  val http: HttpExt = Http(actorSystem)

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
    Await.result(parsedData, 2 seconds)
  }


  /*
//  TODO Fix problem with needing a materializer in runFold
  def entityToByteString(data: Source[ByteString, Any]): Future[ByteString] = data
    .runFold(ByteString.empty){ case (acc,b) => acc ++ b}
   */


  // should we check that the keys in signing keys can be used to sign the message
  // i.e. all the keys have to be unlocked..
  // check that the signing keys match the ones in issuer.

  def signTx(transaction: Json, keyManager: KeyManager, signingKeys: List[String]): Json = {
    val result = (transaction \\ "result").head
    val tx = (result \\ "formattedTx").head
    val messageToSign = (result \\ "messageToSign").head
    assert(signingKeys.contains((tx \\ "issuer").head.asString.get))

    var sigs: List[(String, String)] = signingKeys.map { pk =>
      val pubKey = PublicKey25519Proposition(Base58.decode(pk).get)
      val privKey = keyManager.secrets.find(sk => sk.publicKeyBytes sameElements pubKey.pubKeyBytes)

      privKey match {
            case Some(sk) =>
              val signature = Base58.encode(PrivateKey25519Companion.sign(sk, messageToSign.asString.get.getBytes).signature)
              (pk, signature)
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

  def sendRequest(request: ByteString): Json  = {
    val sendTx = httpPOST(request)
    val data = requestResponseByteString(sendTx)
    byteStringToJSON(data)

  }

}


package example.requests

//Import relevant actor libraries
import akka.actor.Status.{Failure, Success}
import akka.actor.{Actor, ActorLogging, ActorSystem}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.RawHeader
import akka.stream.ActorMaterializer
import akka.util.{ByteString, Timeout}
import example.KeyManager.getListOfFiles
import example.{PrivateKey25519Companion, PublicKey25519Proposition, _}
import io.circe.Json
import io.circe.syntax._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.concurrent.duration._
import scala.util.Try
import scorex.crypto.encode.Base58

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

  // should we check that the keys in signing keys can be used to sign the message
  // i.e. all the keys have to be unlocked..
  // check that the signing keys match the ones in issuer.

  def signTx(transaction: Json, keyManager: KeyManager, signingKeys: List[String], passwords: List[String]): Json = {
    val result = (transaction \\ "result").head
    val tx = (result \\ "formattedTx").head
    val messageToSign = (result \\ "messageToSign").head
    assert(signingKeys.contains((tx \\ "issuer").head.asString.get))
    var sigs: Set[String] = Set()
    signingKeys.map(
      key => {
        val pubKey = PublicKey25519Proposition(Base58.decode(key).get)
        if (keyManager.publicKeys.contains(pubKey)) {
          val keyFile = KeyManager.getListOfFiles(keyManager.defaultKeyDir).map(
            file => KeyFile.readFile(file.getPath))
            .filter(
              k => k.pubKeyBytes sameElements pubKey.pubKeyBytes
            ).head
          passwords.map(
            pswd => {
              if (keyFile.getPrivateKey(pswd).get.isInstanceOf[PrivateKey25519]) {
                val privKey = keyFile.getPrivateKey(pswd).get
                println(privKey.privKeyBytes sameElements keyManager.secrets.head.privKeyBytes)

//                if (keyManager.secrets.contains(privKey)) { //  checking if unlocked
                if (privKey.privKeyBytes sameElements keyManager.secrets.head.privKeyBytes) { // temp fix
                  println("im unlocked")
                  sigs += PrivateKey25519Companion.sign(privKey, messageToSign.asString.get.getBytes).toString
                }
                print(sigs)
              }
            }
          )
        }
      }
    )
    println(sigs)
    val newTx = tx.deepMerge(Map(
      "signatures" -> sigs.toString().asJson
    ).asJson)
    val newResult = Map("formattedTx"-> newTx).asJson
    transaction.deepMerge(
      Map("result" -> newResult).asJson
    )
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


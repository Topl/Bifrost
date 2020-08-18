package example

//Import relevant akka libraries
import akka.actor.ActorSystem
import akka.http.scaladsl.{Http, HttpExt}
import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.stream.ActorMaterializer
import example.requests.Requests
import org.scalatest.{AsyncFlatSpec, Matchers}
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Blake2b256
import io.circe.syntax._

import scala.concurrent.Future
import scala.reflect.io.Path
import scala.util.Try

class RequestSpec extends AsyncFlatSpec with Matchers {

  implicit val actorSystem: ActorSystem = ActorSystem()
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  import actorSystem.dispatcher

  val http: HttpExt = Http(actorSystem)

  val requests = new Requests

  val seed1 = Blake2b256(java.util.UUID.randomUUID.toString)
  val seed2 = Blake2b256(java.util.UUID.randomUUID.toString)
  val (sk1, pk1) = PrivateKey25519Companion.generateKeys(seed1)
  val (sk2, pk2) = PrivateKey25519Companion.generateKeys(seed2)

  val keyFileDir = "keyfiles/keyManagerTest"
  val path: Path = Path(keyFileDir)
  Try(path.deleteRecursively())
  Try(path.createDirectory())
  val password = "pass"

  val keyFile = KeyFile(password, seed1, keyFileDir)
  val keyManager = KeyManager(Set(), keyFileDir)
  keyManager.unlockKeyFile(Base58.encode(sk1.publicKeyBytes), password)

  val amount = 10

  it should "receive a successful response from Bifrost upon creating asset" in {
    val createAssetRequest = requests.transaction("createAssetsPrototype", Base58.encode(pk1.pubKeyBytes), Base58.encode(pk2.pubKeyBytes), amount)

    val sendTx = requests.httpPOST(createAssetRequest)

    val response: Future[HttpResponse] = http.singleRequest(sendTx)

    response.map { res => {
      res.status shouldBe StatusCodes.OK
//      res.entity.dataBytes
      // need to change it to something other than a foreach to return ByteString -> later to be parsed as JSON
      // transforming ByteString to JSON -> should be its own fxn because used a lot (data being received from requests)
      }
    }
    /*response.map {
      case response@HttpResponse(StatusCodes.OK, headers, entity, _) =>
        entity.dataBytes.runFold(ByteString(""))(_ ++ _).foreach { body =>
          println(body.utf8String)
        }
      case response @ HttpResponse(code, _, _, _) =>
        println(code)
    }
     */
  }


  it should "receive JSON from sign transaction" in {
    val issuer: List[String] = List(Base58.encode(pk1.pubKeyBytes))
    val passwords = List("pass")
    lazy val formattedTx = Map(
      "txType"-> "AssetCreation".asJson,
      "txHash"-> "AVUzHAQ1HLp5gSxvTVoLgqdHQjeHsG7rVLxj5Pd6onmF".asJson,
      "signatures"-> {}.asJson,
      "data"-> "".asJson,
      "issuer"->s"${issuer.head}".asJson,
      "assetCode"-> "test_1".asJson,
      "fee"-> 0.asJson
    )
    lazy val result = "result" -> Map (
      "formattedTx" -> formattedTx.asJson,
      "messageToSign" -> "test-message".asJson
    )
    lazy val transaction = Map(
      "jsonrpc" -> "2.0".asJson,
      "id" -> "1".asJson,
      "method" -> "signTx".asJson,
      "result" -> Map (
        "formattedTx" -> formattedTx.asJson,
        "messageToSign" -> "test-message".asJson
      ).asJson
    ).asJson
    print(transaction)
    val JSON = requests.signTx(transaction, keyManager, issuer, passwords)
    print(JSON)
    assert((JSON \\ "signatures").head != null)
  }

//  "broadcast transaction" {
//
//  }
}

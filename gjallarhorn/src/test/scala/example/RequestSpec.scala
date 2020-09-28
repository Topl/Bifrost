package example

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.{Http, HttpExt}
import akka.stream.ActorMaterializer
import _root_.requests.Requests
import akka.util.ByteString
import crypto.PrivateKey25519Companion
import io.circe.Json
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Blake2b256
import keymanager.{KeyFile, Keys}

import scala.reflect.io.Path
import scala.util.Try

class RequestSpec extends AsyncFlatSpec
  with Matchers
  with GjallarhornGenerators {

  implicit val actorSystem: ActorSystem = ActorSystem()
  implicit val materializer: ActorMaterializer = ActorMaterializer()

  val http: HttpExt = Http(actorSystem)

  val requests = new Requests(settings)

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
  val keyManager = Keys(Set(), keyFileDir)
  keyManager.unlockKeyFile(Base58.encode(sk1.publicKeyBytes), password)


  val amount = 10

  var transaction: Json = Json.Null
  var signedTransaction: Json = Json.Null

  it should "receive a successful response from Bifrost upon creating asset" in {
    val createAssetRequest: ByteString = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "2",
         |   "method": "createAssetsPrototype",
         |   "params": [{
         |     "issuer": "${Base58.encode(pk1.pubKeyBytes)}",
         |     "recipient": "${Base58.encode(pk2.pubKeyBytes)}",
         |     "amount": $amount,
         |     "assetCode": "etherAssets",
         |     "fee": 0,
         |     "data": ""
         |   }]
         |}
         """.stripMargin)
    transaction = requests.sendRequest(createAssetRequest, "asset")
    assert(transaction.isInstanceOf[Json])
  }

  it should "receive JSON from sign transaction" in {
    val issuer: List[String] = List(Base58.encode(pk1.pubKeyBytes))
    signedTransaction = requests.signTx(transaction, keyManager, issuer)
    val sigs = (signedTransaction \\ "signatures").head.asObject.get
    issuer.foreach(key => assert(sigs.contains(key)))

    assert((signedTransaction \\ "signatures").head.asObject.isDefined)
  }

  it should "receive JSON from broadcast transaction" in {
    val response = requests.broadcastTx(signedTransaction)
    assert(response.isInstanceOf[Json])
  }


}

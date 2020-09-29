package example

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.http.scaladsl.{Http, HttpExt}
import akka.stream.ActorMaterializer
import _root_.requests.Requests
import akka.util.{ByteString, Timeout}
import crypto.PrivateKey25519Companion
import io.circe.Json
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Blake2b256
import keymanager.{KeyFile, Keys}
import wallet.WalletManager
import wallet.WalletManager._

import scala.reflect.io.Path
import scala.util.Try
import scala.concurrent.Await
import scala.collection.mutable.{Map => MMap}
import scala.concurrent.duration._


class RequestSpec extends AsyncFlatSpec
  with Matchers
  with GjallarhornGenerators {

  implicit val actorSystem: ActorSystem = ActorSystem()
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val timeout: Timeout = 10.seconds

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

  val publicKeys: Set[String] = Set(Base58.encode(pk1.pubKeyBytes), Base58.encode(pk2.pubKeyBytes))

  val walletManagerRef: ActorRef = actorSystem.actorOf(Props(new WalletManager(publicKeys)))

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
    (transaction \\ "error").isEmpty shouldBe true
    (transaction \\ "result").head.asObject.isDefined shouldBe true
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
    (response \\ "error").isEmpty shouldBe true
    (response \\ "result").head.asObject.isDefined shouldBe true
  }

  var balanceResponse: Json = Json.Null

  it should "receive JSON from getBalances" in {
    var pubKeys: Set[String] = Set.empty
    pubKeys += Base58.encode(pk1.pubKeyBytes)
    pubKeys += Base58.encode(pk2.pubKeyBytes)
    balanceResponse = requests.getBalances(pubKeys)
    assert(balanceResponse.isInstanceOf[Json])
    (balanceResponse \\ "error").isEmpty shouldBe true
    (balanceResponse \\ "result").head.asObject.isDefined shouldBe true
  }

  it should "update boxes correctly with balance response" in {
    val walletBoxes: MMap[String, MMap[String, Json]] = Await.result((walletManagerRef ? UpdateWallet((balanceResponse \\ "result").head))
      .mapTo[MMap[String, MMap[String, Json]]]
      , 10.seconds)
    val pubKeyBoxes: Option[MMap[String, Json]] = walletBoxes.get(Base58.encode(pk1.pubKeyBytes))
    pubKeyBoxes match {
      case Some(map) => assert(map.keySet.isEmpty)
      case None => sys.error("something wrong")
    }
  }


}

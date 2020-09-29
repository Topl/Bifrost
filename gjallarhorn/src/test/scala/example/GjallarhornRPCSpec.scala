package example

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{HttpEntity, HttpMethods, HttpRequest, MediaTypes}
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.{Http, HttpExt}
import akka.util.{ByteString, Timeout}
import akka.pattern.ask
import crypto.PrivateKey25519Companion
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import http.GjallarhornApiRoute
import akka.http.scaladsl.testkit.ScalatestRouteTest
import io.circe.Json
import io.circe.parser.parse
import keymanager.{KeyManagerRef, Keys}
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Blake2b256
import wallet.WalletManager

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.collection.mutable.{Map => MMap}

class GjallarhornRPCSpec extends AsyncFlatSpec
  with Matchers
  with ScalatestRouteTest
  with GjallarhornGenerators {

  implicit val actorSystem: ActorSystem = ActorSystem()
//  implicit val materializer: ActorMaterializer = ActorMaterializer()

  implicit val timeout: Timeout = Timeout(10.seconds)
  val http: HttpExt = Http(actorSystem)

  val seed1 = Blake2b256(java.util.UUID.randomUUID.toString)
  val seed2 = Blake2b256(java.util.UUID.randomUUID.toString)
  val (sk1, pk1) = PrivateKey25519Companion.generateKeys(seed1)
  val (sk2, pk2) = PrivateKey25519Companion.generateKeys(seed2)

  val amount = 10

  val keyManagerRef: ActorRef = KeyManagerRef("keyManager", "keyfiles")
  val keyFileDir = "keyfiles/keyManagerTest"
  val keyManager = Keys(Set(), keyFileDir)
  val walletManagerRef: ActorRef = system.actorOf(Props(new WalletManager(keyManager.listOpenKeyFiles)))

  /*val testPubKey = keyManager.publicKeys.head.toString
  val testPubKey2 = keyManager.publicKeys.tail.head.toString*/

  val route: Route = GjallarhornApiRoute(settings, keyManagerRef, walletManagerRef).route

  def httpPOST(jsonRequest: ByteString): HttpRequest = {
    HttpRequest(
      HttpMethods.POST,
      uri = "/gjallarhorn/",
      entity = HttpEntity(MediaTypes.`application/json`, jsonRequest)
    ).withHeaders(RawHeader("x-api-key", "test_key"))
  }

  it should "get a successful JSON response" in {
    val createAssetRequest = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "2",
         |   "method": "createTransaction",
         |   "params": [{
         |     "method": "createAssetsPrototype",
         |     "params": [{
         |        "issuer": "${Base58.encode(pk1.pubKeyBytes)}",
         |        "recipient": "${Base58.encode(pk2.pubKeyBytes)}",
         |        "amount": $amount,
         |        "assetCode": "etherAssets",
         |        "fee": 0,
         |        "data": ""
         |     }]
         |   }]
         |}
         """.stripMargin)

    httpPOST(createAssetRequest) ~> route ~> check {
      val res = parse(responseAs[String]).right.get
      (res \\ "error").isEmpty shouldBe true
      (res \\ "result").head.asObject.isDefined shouldBe true
    }
  }


  /*
  it should "add 1 box after transaction was broadcasted" in {
    val broadcastTransaction = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "1",
         |   "method": "broadcastTx",
         |   "params": [{
         |      "tx": {
         |         "signatures": {
         |             "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ": "oNvbqGatNR2aZTneYcF8JsmUDb1emh64FSvfN7Svf9t6edqGgEVYNLBebJrcCGXarr1HGUVQnLgVFysyyjU5wZa"
         |          },
         |          "txType": "AssetCreation",
         |          "txHash": "3Z5SzHiCuHKPdn8wypN8GuhnWJkSL2ZtRVbJq4a1jLry",
         |          "timestamp": 1586474743821,
         |          "newBoxes": [
         |               "5kSiDeEJLHSk52NxM5he5MzFyTSPKHwonuPsyXrnRYRw"
         |          ],
         |          "data": "",
         |          "issuer": "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ",
         |          "to": [
         |              [
         |                "$testPubKey",
         |                "10"
         |              ]
         |           ],
         |           "assetCode": "test",
         |           "fee": 0
         |       }
         |   }]
         |}
         """.stripMargin)
    httpPOST(broadcastTransaction) ~> route ~> check {
      val res = parse(responseAs[String]).right.get
      (res \\ "error").isEmpty shouldBe false
      (res \\ "result").head.asObject.isDefined shouldBe true
    }
    val walletBoxes: MMap[String, MMap[String, Json]] = Await.result((walletManagerRef ? GetWalletBoxes()).mapTo[MMap[String, MMap[String, Json]]],
      10.seconds)
    assert(walletBoxes.get(testPubKey).size == 1)
  }

  it should "add and remove correct boxes from wallet" in {
    val transaction = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "1",
         |   "method": "broadcastTx",
         |   "params": [{
         |      "tx" : {
         |          "txType" : "AssetTransfer",
         |          "txHash" : "91rmGL6KdsgZ16pu3Hbc3RRuPqiTBs48imnhQnjTTtfP",
         |          "timestamp" : 1601392939391,
         |          "signatures" : {
         |              "F6ABtYMsJABDLH2aj7XVPwQr5mH7ycsCE4QGQrLeB3xU" : "2UpYvWSwXdVs6MqXUd9neULTrvwTANH3ZRL3rJBg1JR22EmaZRZxyC8wqvEUVDuyXbU7BvJxZFr4XTmw7vg78dQq"
         |          },
         |          "newBoxes" : [
         |              "2d4r8oRBq4xdwCaBBeF7pZnfV672crivXpDJ9wY31CaN",
         |              "3x7daFS1DsC2EENw8T3R6CBzQqS9qDyQxstxvoyJLCUs"
         |          ],
         |          "data" : "",
         |          "issuer" : "F6ABtYMsJABDLH2aj7XVPwQr5mH7ycsCE4QGQrLeB3xU",
         |          "to" : [
         |            [
         |              "$testPubKey",
         |              "9"
         |            ],
         |            [
         |              "$testPubKey2",
         |              "1"
         |              ]
         |          ],
         |          "assetCode" : "etherAssets",
         |          "from" : [
         |            [
         |              "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ",
         |              "6917471470825143755"
         |            ]
         |          ],
         |          "boxesToRemove" : [
         |            "5kSiDeEJLHSk52NxM5he5MzFyTSPKHwonuPsyXrnRYRw"
         |          ],
         |          "fee" : 0
         |        }
         |     }]
         |}
         """.stripMargin)

    httpPOST(transaction) ~> route ~> check {
      val res = parse(responseAs[String]).right.get
      (res \\ "error").isEmpty shouldBe false
      (res \\ "result").head.asObject.isDefined shouldBe true
    }
    val walletBoxes: MMap[String, MMap[String, Json]] = Await.result((walletManagerRef ? GetWalletBoxes()).mapTo[MMap[String, MMap[String, Json]]],
      10.seconds)
    System.out.println(walletBoxes)
    assert(walletBoxes.get(testPubKey).size == 1)
    assert(walletBoxes.get(testPubKey2).size == 1)
  }*/

}

package example

import akka.actor.{ActorRef, ActorSelection, ActorSystem, DeadLetter, Props}
import akka.pattern.ask
import akka.http.scaladsl.{Http, HttpExt}
import akka.stream.ActorMaterializer
import _root_.requests.Requests
import akka.util.{ByteString, Timeout}
import crypto.PrivateKey25519Companion
import io.circe.Json
import io.circe.parser.parse
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.util.encode.Base58
import scorex.crypto.hash.Blake2b256
import keymanager.{KeyFile, Keys}
import wallet.{DeadLetterListener, WalletManager}
import wallet.WalletManager._

import scala.reflect.io.Path
import scala.util.{Failure, Success, Try}
import scala.concurrent.Await
import scala.collection.mutable.{Map => MMap}
import scala.concurrent.duration._


class RequestSpec extends AsyncFlatSpec
  with Matchers
  with GjallarhornGenerators {

  implicit val actorSystem: ActorSystem = ActorSystem("requestTest", requestConfig)
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val timeout: Timeout = 10.seconds

  val http: HttpExt = Http(actorSystem)

  val requests = new Requests(requestSettings)

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

  val publicKeys: Set[String] = Set(Base58.encode(pk1.pubKeyBytes), Base58.encode(pk2.pubKeyBytes), "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ")

  val walletManagerRef: ActorRef = actorSystem.actorOf(Props(new WalletManager(publicKeys)), name = "WalletManager")

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
         |     "issuer": "${publicKeys.head}",
         |     "recipient": "${publicKeys.tail.head}",
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


  it should "receive successful JSON response from sign transaction" in {
    val issuer: List[String] = List(publicKeys.head)
    signedTransaction = requests.signTx(transaction, keyManager, issuer)
    val sigs = (signedTransaction \\ "signatures").head.asObject.get
    issuer.foreach(key => assert(sigs.contains(key)))
    assert((signedTransaction \\ "signatures").head.asObject.isDefined)
    (signedTransaction \\ "error").isEmpty shouldBe true
    (signedTransaction \\ "result").head.asObject.isDefined shouldBe true
  }

  it should "receive successful JSON response from broadcast transaction" in {
    val response = requests.broadcastTx(signedTransaction)
    assert(response.isInstanceOf[Json])
    (response \\ "error").isEmpty shouldBe true
    (response \\ "result").head.asObject.isDefined shouldBe true
  }

  it should "receive a successful response from Bifrost upon transfering a poly" in {
    val transferPolysRequest: ByteString = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "1",
         |   "method": "transferPolys",
         |   "params": [{
         |     "recipient": "${publicKeys.tail.head}",
         |     "sender": ["6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ"],
         |     "amount": $amount,
         |     "fee": 0,
         |     "data": ""
         |   }]
         |}
         """.stripMargin)
    val tx = requests.sendRequest(transferPolysRequest, "wallet")
    assert(tx.isInstanceOf[Json])
    (transaction \\ "error").isEmpty shouldBe true
    (transaction \\ "result").head.asObject.isDefined shouldBe true
  }

  var balanceResponse: Json = Json.Null
  var newBoxId: String = ""

  def parseForBoxId(json: Json): String = {
    val result = (json \\ "result").head
    val newBoxes = (result \\ "newBoxes").head.toString().trim.stripPrefix("[").stripSuffix("]").trim
    newBoxes
  }

  it should "receive a successful and correct response from Bifrost upon requesting balances" in {
    val createAssetRequest: ByteString = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "1",
         |   "method": "createAssets",
         |   "params": [{
         |     "issuer": "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ",
         |     "recipient": "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ",
         |     "amount": $amount,
         |     "assetCode": "test",
         |     "fee": 0,
         |     "data": ""
         |   }]
         |}
         """.stripMargin)
    transaction = requests.sendRequest(createAssetRequest, "asset")
    newBoxId = parseForBoxId(transaction)
    Thread.sleep(10000)
    balanceResponse = requests.getBalances(publicKeys)
    assert(balanceResponse.isInstanceOf[Json])
    (balanceResponse \\ "error").isEmpty shouldBe true
    val result: Json = (balanceResponse \\ "result").head
    result.asObject.isDefined shouldBe true
    (((result \\ "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ").head \\ "Boxes").head \\ "Asset").
      head.toString().contains(newBoxId) shouldBe true
  }

  it should "update boxes correctly with balance response" in {
    val walletBoxes: MMap[String, MMap[String, Json]] = Await.result((walletManagerRef ? UpdateWallet((balanceResponse \\ "result").head))
      .mapTo[MMap[String, MMap[String, Json]]], 10.seconds)

    val pubKeyEmptyBoxes: Option[MMap[String, Json]] = walletBoxes.get(publicKeys.head)
    pubKeyEmptyBoxes match {
      case Some(map) => assert(map.keySet.isEmpty)
      case None => sys.error(s"no mapping for given public key: ${publicKeys.head}")
    }

   val pubKeyWithBoxes: Option[MMap[String, Json]] = walletBoxes.get("6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ")
    pubKeyWithBoxes match {
      case Some(map) => {
        val firstBox: Option[Json] = map.get(newBoxId)
        firstBox match {
          case Some(json) => assert ((json \\ "value").head.toString() == "\"10\"")
          case None => sys.error("no keys in mapping")
        }
      }
      case None => sys.error("no mapping for given public key: 6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ")
    }
  }

  /*it should "connect to bifrost actor when the gjallarhorn app starts" in {
    val bifrostActor: ActorRef = Await.result(actorSystem.actorSelection(
      "akka.tcp://bifrost-client@127.0.0.1:9087/user/walletActorManager").resolveOne(), 10.seconds)
    walletManagerRef ! GjallarhornStarted(bifrostActor)
    Thread.sleep(10000)
    assert(bifrostResponse.contains("received new wallet from: Actor[akka.tcp://requestTest@127.0.0.1"))

    val newBlock: Option[String] = Await.result((walletManagerRef ? GetNewBlock).mapTo[Option[String]], 100.seconds)
    newBlock match {
      case Some(block) => assert(block == "new block")
      case None => sys.error("no new blocks")
    }
  }*/

  it should "send msg to bifrost actor when the gjallarhorn app stops" in {
    val bifrostActor: ActorRef = Await.result(actorSystem.actorSelection(
      "akka.tcp://bifrost-client@127.0.0.1:9087/user/walletActorManager").resolveOne(), 10.seconds)
    walletManagerRef ! GjallarhornStarted(bifrostActor)
    Thread.sleep(1000)
    val bifrostResponse: String = Await.result((walletManagerRef ? GjallarhornStopped).mapTo[String], 100.seconds)
    assert(bifrostResponse.contains("The remote wallet Actor[akka.tcp://requestTest@127.0.0.1") &&
      bifrostResponse.contains("has been removed from the WalletActorManager in Bifrost"))
  }

  it should "update wallet correctly after receiving new block" in {
    val block: String =
      s"""
         |{
         |  "timestamp" : 1602624092235,
         |  "signature" : "3HNmKwuvGAzBKjQcK79C5RQRkvNimpP11t2wPqsgAF5AEhoo679dndndor5PiLQG1XDgTuao3mBbEftZgogVCM9H",
         |  "blockSize" : 316,
         |  "generatorBox" : "fc9Lb6RcnoJ5A4vzyHNezNHFtwytxVRk9z1w7FRU8eeFFeB1NuZy8Rk9jmofc4i2aaq49CqkS",
         |  "version" : 1,
         |  "id" : "9ZP1eUEDAhtesHZH4PFaaFhjyo9FvFqA8QGSH2auoJbX",
         |  "txs" : [
         |    {
         |      "txType" : "CoinbaseTransaction",
         |      "txHash" : "3nKBueQ1Mj5VkAiqNf9LgUsT5sLJLUpVXmk5otwYmU52",
         |      "timestamp" : 1602624092222,
         |      "signatures" : [
         |        "4uqAkTgy3xvEs7ipVJUjbA36xgjAHb9D9LXBWUDK8iaBzniPkLi38G5gJknbhTK7qTSTytZi1aEcpcqP6CiFHwWV"
         |      ],
         |      "newBoxes" : [
         |      ],
         |      "to" : [
         |        {
         |          "proposition" : "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ",
         |          "value" : 0
         |        }
         |      ],
         |      "fee" : 0
         |    }
         |  ],
         |  "parentId" : "4RbYjQHVWRcvTEoJcpFNfBrcNL7tpMatP13Tr8fJMHDm"
         |}
         """
    parse(block) match {
      case Right(blockJson) => {
        val actorMsg: String = s"new block added: $blockJson"
        walletManagerRef ! actorMsg
        Thread.sleep(1000)
        val walletBoxes: MMap[String, MMap[String, Json]] = Await.result((walletManagerRef ? GetWallet)
          .mapTo[MMap[String, MMap[String, Json]]], 10.seconds)
        val iTguyBoxes: Option[MMap[String, Json]] = walletBoxes.get("6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ")
        iTguyBoxes match {
          case Some(map) => {
            assert(map.size == 1)
          }
          case None => sys.error("no mapping for given public key: 6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ")
        }
      }
      case Left(e) => sys.error(s"Could not parse json $e")
    }
  }


}

package example

import akka.actor.{ActorRef, ActorSelection, ActorSystem, DeadLetter, Props}
import akka.pattern.ask
import akka.http.scaladsl.{Http, HttpExt}
import akka.stream.{ActorMaterializer, Materializer}
import _root_.requests.{Requests, RequestsManager}
import akka.util.{ByteString, Timeout}
import crypto.{PrivateKey25519Companion, PublicKey25519Proposition}
import io.circe.{Json, parser}
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
import scala.concurrent.{Await, ExecutionContextExecutor, Future}
import scala.collection.mutable.{Map => MMap}
import scala.concurrent.duration._


class RequestSpec extends AsyncFlatSpec
  with Matchers
  with GjallarhornGenerators {

  implicit val actorSystem: ActorSystem = ActorSystem("requestTest", requestConfig)
  implicit val context: ExecutionContextExecutor = actorSystem.dispatcher
  implicit val timeout: Timeout = 30.seconds

  val http: HttpExt = Http(actorSystem)

  val requestsManagerRef: ActorRef = actorSystem.actorOf(Props(new RequestsManager), name = "RequestsManager")

  val requests = new Requests(requestSettings, requestsManagerRef)

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
    val response = Await.result(requests.broadcastTx(signedTransaction), 10.seconds)
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
         |   "method": "transferPolysPrototype",
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
         |   "method": "createAssetsPrototype",
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
    println(transaction)
    newBoxId = parseForBoxId(transaction)
    Thread.sleep(10000)
    balanceResponse = requests.getBalances(publicKeys)
    assert(balanceResponse.isInstanceOf[Json])
    (balanceResponse \\ "error").isEmpty shouldBe true
    val result: Json = (balanceResponse \\ "result").head
    result.asObject.isDefined shouldBe true
    println (result)
    (((result \\ "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ").head \\ "Boxes").head \\ "Asset").
      head.toString().contains(newBoxId) shouldBe true
  }
/*
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
*/
  it should "connect to bifrost actor when the gjallarhorn app starts" in {
    val bifrostActor: ActorRef = Await.result(actorSystem.actorSelection(
      "akka.tcp://bifrost-client@127.0.0.1:9087/user/walletConnectionHandler").resolveOne(), 10.seconds)
    walletManagerRef ! GjallarhornStarted(bifrostActor)
    Thread.sleep(10000)

    val newBlock: Option[String] = Await.result((walletManagerRef ? GetNewBlock).mapTo[Option[String]], 100.seconds)
    newBlock match {
      case Some(block) => assert(block.contains("timestamp") && block.contains("signature") && block.contains("id") && block.contains("newBoxes"))
      case None => sys.error("no new blocks")
    }
  }

 it should "send msg to bifrost actor when the gjallarhorn app stops" in {
    val bifrostResponse: String = Await.result((walletManagerRef ? GjallarhornStopped).mapTo[String], 100.seconds)
    assert(bifrostResponse.contains("The remote wallet Actor[akka.tcp://requestTest@127.0.0.1") &&
      bifrostResponse.contains("has been removed from the WalletConnectionHandler in Bifrost"))
  }

  it should "update wallet correctly after receiving new block" in {
    val block: ByteString = ByteString(
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
         |      "txType" : "ArbitTransfer",
         |      "txHash" : "3nKBueQ1Mj5VkAiqNf9LgUsT5sLJLUpVXmk5otwYmU52",
         |      "timestamp" : 1602624092222,
         |      "signatures" : [
         |        "4uqAkTgy3xvEs7ipVJUjbA36xgjAHb9D9LXBWUDK8iaBzniPkLi38G5gJknbhTK7qTSTytZi1aEcpcqP6CiFHwWV"
         |      ],
         |      "newBoxes" : [
         |        {
         |          "id" : "9asdjeokMDFG8sasdd6JKL0wj",
         |          "type" : "Arbit",
         |          "proposition" : "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ",
         |          "value" : "10",
         |          "nonce" : "mhj3GHI9asdkj0JKLre3P"
         |        },
         |        {
         |          "nonce": "-9110370178208068175",
         |          "id": "GGDsEQdd5cnbgjKkac9HLpp2joGo6bWgmS2KvhJgd8b8",
         |          "type": "Arbit",
         |          "proposition": "3X4AW3Swr1iM1syu2g4Xi4L4eTSJFKxGsZPgVctUYg4ga8MZpD",
         |          "value": "1000000"
         |        },
         |        {
         |          "nonce": "-8269943573030898832",
         |          "id": "97gkUUwPQWGKU1LMf7cZE4PGdbUezWYLcbcuiRRryGeE",
         |          "type": "Arbit",
         |          "proposition": "4EoSC4YmTm7zoPt5HDJU4aa73Vn2LPrmUszvggAPM5Ff3R1DVt",
         |          "value": "1000000"
         |        }
         |      ],
         |      "to" : [
         |        {
         |          "proposition" : "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ",
         |          "value" : 0
         |        }
         |      ],
         |      "boxesToRemove": [
         |                        "2fvgQ6xAJbMxtsGv73veyN3sHnwKUh2Lda3b9CyNxriv"
         |      ],
         |      "fee" : 0
         |    },
         |    {
         |      "txType": "PolyTransfer",
         |      "txHash": "G1KX8RPVBBmHWuuZ7ihNkQLXVJa8AMr4DxafAJHUUCuy",
         |      "timestamp": 0,
         |      "signatures": {
         |          "2xdTv8awN1BjgYEw8W1BVXVtiEwG2b29U8KoZQqJrDuEqSQ9e4": "Signature25519(2AXDGYSE4f2sz7tvMMzyHvUfcoJmxudvdhBcmiUSo6ijwfYmfZYsKRxboQMPh3R4kUhXRVdtSXFXMheka4Rc4P2)"
         |      },
         |      "newBoxes": [
         |          {
         |             "nonce": "-5988475187915922381",
         |             "id": "GgNqzkSywewv99vCrb99UakEw1Myn4mqYXo3N4a6PWVW",
         |             "type": "Poly",
         |             "proposition": "3X4AW3Swr1iM1syu2g4Xi4L4eTSJFKxGsZPgVctUYg4ga8MZpD",
         |             "value": "1000000"
         |          },
         |          {
         |             "nonce": "965750754031143229",
         |             "id": "5UGTHuvG7kJVqp9Sw55A1C6wVEtgeQKn12njLG1bbUTK",
         |             "type": "Poly",
         |             "proposition": "4EoSC4YmTm7zoPt5HDJU4aa73Vn2LPrmUszvggAPM5Ff3R1DVt",
         |             "value": "1000000"
         |          },
         |          {
         |             "nonce": "-59884751870915922381",
         |             "id": "GgNqzkSywewv10vCrb99UakEw1Myn5mqYXo3N4a6PWVW",
         |             "type": "Poly",
         |             "proposition": "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ",
         |             "value": "1000000"
         |          }
         |      ],
         |      "to" : [
         |        {
         |          "proposition" : "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ",
         |          "value" : 0
         |        }
         |      ],
         |      "boxesToRemove": [
         |                        "2fvgQ6xAJbMxtsGv73veyN3sHnwKUh2Lda3b9CyNxriv"
         |      ],
         |      "fee" : 0
         |    }
         |  ],
         |  "parentId" : "4RbYjQHVWRcvTEoJcpFNfBrcNL7tpMatP13Tr8fJMHDm"
         |}
         """.stripMargin)
    parser.parse(block.utf8String) match {
      case Right(blockJson) => {
        walletManagerRef ! s"new block added: $blockJson"
        Thread.sleep(1000)
        val walletBoxes: MMap[String, MMap[String, Json]] = Await.result((walletManagerRef ? GetWallet)
          .mapTo[MMap[String, MMap[String, Json]]], 10.seconds)
        val iTguyBoxes: Option[MMap[String, Json]] = walletBoxes.get("6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ")
        iTguyBoxes match {
          case Some(map) => {
            assert(map.size == 2)
          }
          case None => sys.error("no mapping for given public key: 6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ")
        }
      }
      case Left(e) => sys.error(s"Could not parse json $e")
    }
  }

}

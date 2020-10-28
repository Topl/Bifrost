package example

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.http.scaladsl.{Http, HttpExt}
import _root_.requests.{Requests, RequestsManager}
import akka.util.{ByteString, Timeout}
import crypto.{PrivateKey25519, PublicKey25519Proposition}
import io.circe.{Json, parser}
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.crypto.hash.Blake2b256
import keymanager.{KeyFile, Keys}
import wallet.WalletManager
import wallet.WalletManager._

import scala.reflect.io.Path
import scala.util.{Failure, Success, Try}
import scala.concurrent.{Await, ExecutionContextExecutor}
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

  val seed1: Array[Byte] = Blake2b256(java.util.UUID.randomUUID.toString)
  val seed2: Array[Byte] = Blake2b256(java.util.UUID.randomUUID.toString)
  val seed3: Array[Byte] = Blake2b256(java.util.UUID.randomUUID.toString)
  //val (sk1, pk1): (PrivateKey25519, PublicKey25519Proposition) = PrivateKey25519.generateKeys(seed1)

  val (sk2, pk2): (PrivateKey25519, PublicKey25519Proposition) = PrivateKey25519.generateKeys(seed2)
  val (sk3, pk3): (PrivateKey25519, PublicKey25519Proposition) = PrivateKey25519.generateKeys(seed3)
  var pk1: PublicKey25519Proposition = pk2

  val keyFileDir = "keyfiles/keyManagerTest"
  val path: Path = Path(keyFileDir)
  Try(path.deleteRecursively())
  Try(path.createDirectory())
  val password = "pass"
  val genesisPubKey = "3mBVXE3fuVfu1MBmMsBiRd6y9XokiVV591N65tBfA3FEvmqWB8"

  val keyFile = KeyFile(password, KeyFile.generateKeyPair(seed1)._1)
  val keyManager = Keys(keyFileDir)
  //keyManager.unlockKeyFile(Base58.encode(sk1.publicKeyBytes), password)
  keyManager.generateKeyFile(password) match {
    case Success(pk) => pk1 = pk
    case Failure (ex) => throw new Error (s"An error occured: $ex")
  }

  val publicKeys: Set[String] = Set(pk1.toString, pk2.toString, pk3.toString, genesisPubKey)

  val walletManagerRef: ActorRef = actorSystem.actorOf(Props(new WalletManager(publicKeys)), name = "WalletManager")

  val amount = 10

  var transaction: Json = Json.Null
  var signedTransaction: Json = Json.Null

  var newBoxId: String = ""

  def parseForBoxId(json: Json): String = {
    val result = (json \\ "result").head
    val newBoxes = (result \\ "newBoxes").head.toString().trim.stripPrefix("[").stripSuffix("]").trim
    newBoxes
  }


 it should "receive a successful response from Bifrost upon creating asset" in {
   val createAssetRequest: ByteString = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "2",
         |   "method": "createAssetsPrototype",
         |   "params": [{
         |     "issuer": "${pk1.toString}",
         |     "recipient": "${pk1.toString}",
         |     "amount": $amount,
         |     "assetCode": "etherAssets",
         |     "fee": 0,
         |     "data": ""
         |   }]
         |}
         """.stripMargin)
    transaction = requests.sendRequest(createAssetRequest, "asset")
    assert(transaction.isInstanceOf[Json])
    newBoxId = parseForBoxId(transaction)
   println(transaction)
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

  var balanceResponse: Json = Json.Null

  it should "receive a successful and correct response from Bifrost upon requesting balances" in {
    Thread.sleep(10000)
    println("pk1: " + pk1.toString)
    println("pk2: " + pk2.toString)
    balanceResponse = requests.getBalances(publicKeys)
    assert(balanceResponse.isInstanceOf[Json])
    (balanceResponse \\ "error").isEmpty shouldBe true
    val result: Json = (balanceResponse \\ "result").head
    result.asObject.isDefined shouldBe true
    println (result)
    (((result \\ pk1.toString).head \\ "Boxes").head \\ "Asset").
      head.toString().contains(newBoxId) shouldBe true
  }

  it should "update boxes correctly with balance response" in {
    val walletBoxes: MMap[String, MMap[String, Json]] = Await.result((walletManagerRef ? UpdateWallet((balanceResponse \\ "result").head))
      .mapTo[MMap[String, MMap[String, Json]]], 10.seconds)

    val pubKeyEmptyBoxes: Option[MMap[String, Json]] = walletBoxes.get(pk2.toString)
    pubKeyEmptyBoxes match {
      case Some(map) => assert(map.keySet.isEmpty)
      case None => sys.error(s"no mapping for given public key: ${pk1.toString}}")
    }

    val pubKeyWithBoxes: Option[MMap[String, Json]] = walletBoxes.get(pk1.toString)
    pubKeyWithBoxes match {
      case Some(map) => {
        val firstBox: Option[Json] = map.get(newBoxId)
        firstBox match {
          case Some(json) => assert ((json \\ "value").head.toString() == "\"10\"")
          case None => sys.error("no keys in mapping")
        }
      }
      case None => sys.error(s"no mapping for given public key: ${pk1.toString}")
    }
  }

  it should "receive a successful response from Bifrost upon transfering a poly" in {
    val transferPolysRequest: ByteString = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "1",
         |   "method": "transferPolysPrototype",
         |   "params": [{
         |     "recipient": "${pk3.toString}",
         |     "sender": ["$genesisPubKey"],
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


  it should "connect to bifrost actor when the gjallarhorn app starts" in {
    val bifrostActor: ActorRef = Await.result(actorSystem.actorSelection(
      "akka.tcp://bifrost-client@127.0.0.1:9087/user/walletConnectionHandler").resolveOne(), 10.seconds)
    walletManagerRef ! GjallarhornStarted(bifrostActor)
    Thread.sleep(100)
    val connected = Await.result((walletManagerRef ? IsConnected).mapTo[Boolean], 10.seconds)
    assert(connected)
  }

  it should "receive a block from bifrost after 5000ms" in {
    Thread.sleep(5000)
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
         |   "timestamp" : 1603750438238,
         |   "signature" : "Signature25519(4FjrDM9uUue7syi4xXNH2PC65bDuZStQvU6QV2qmeUJ81DJEf3XMVzwxKyQzyWs2WWvXoWG9nLh5nD7ND1ShtEaa)",
         |   "blockSize" : 335,
         |   "generatorBox" : {
         |      "nonce" : "-439024012794791436",
         |      "id" : "44SWAsPAUdRgm68qgo9joNM9BiRnDfJvQsQNRaA6oLVi",
         |      "type" : "Arbit",
         |      "proposition" : "4YCxsBZujUFEfEaRWhhURgzvWEzN8BWbbho1qEovmhunN7c9fQ",
         |      "value" : "1000000"
         |   },
         |   "version" : 0,
         |   "id" : "CzrMgZskNAwoEE1F65yh7i8WMbYSW6bN3HMRbLKGQ5re",
         |   "txs" : [
         |      {
         |        "txType" : "Coinbase",
         |        "txHash" : "HK5CxRpT1xXBbLeQRnxzBfMZkTr2czwbEcCchRofsx9z",
         |        "timestamp" : 1603750438238,
         |        "signatures" : {
         |          "4YCxsBZujUFEfEaRWhhURgzvWEzN8BWbbho1qEovmhunN7c9fQ" : "Signature25519(5h4GoyqCYC8qhvzDHoheD442JSu7YaE5bNFFemfVhfwGLZtvUYGgXyZ35jYyhG2YtUAW6pnwpkDVdgq2GpAux3xS)"
         |        },
         |        "newBoxes" : [
         |          {
         |            "nonce": "-9110370178208068175",
         |            "id": "GGDsEQdd5cnbgjKkac9HLpp2joGo6bWgmS2KvhJgd8b8",
         |            "type": "Arbit",
         |            "proposition": "${pk1.toString}",
         |            "value": "1000000"
         |          },
         |          {
         |            "nonce": "-8269943573030898832",
         |            "id": "97gkUUwPQWGKU1LMf7cZE4PGdbUezWYLcbcuiRRryGeE",
         |            "type": "Arbit",
         |            "proposition": "4EoSC4YmTm7zoPt5HDJU4aa73Vn2LPrmUszvggAPM5Ff3R1DVt",
         |            "value": "1000000"
         |          }
         |        ],
         |        "to" : [
         |          [
         |            "4Nb1ewkxoT8GJAZkCLaetQAWgVdfcu58v5Uka8iag3nsXakiVj",
         |            0
         |          ]
         |        ],
         |        "parentId" : "HVa5fBayzLEDStb9Hwthe9HDJWAtakGW3o11s9z2cRo4",
         |        "fee" : 0
         |      },
         |      {
         |        "txType": "PolyTransfer",
         |        "txHash": "G1KX8RPVBBmHWuuZ7ihNkQLXVJa8AMr4DxafAJHUUCuy",
         |        "timestamp": 0,
         |        "signatures": {
         |          "2xdTv8awN1BjgYEw8W1BVXVtiEwG2b29U8KoZQqJrDuEqSQ9e4": "Signature25519(2AXDGYSE4f2sz7tvMMzyHvUfcoJmxudvdhBcmiUSo6ijwfYmfZYsKRxboQMPh3R4kUhXRVdtSXFXMheka4Rc4P2)"
         |        },
         |        "newBoxes": [
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
         |             "proposition": "${pk1.toString}",
         |             "value": "1000000"
         |          }
         |        ],
         |        "to" : [
         |          {
         |            "proposition" : "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ",
         |            "value" : 0
         |          }
         |        ],
         |        "boxesToRemove": [
         |                        "2fvgQ6xAJbMxtsGv73veyN3sHnwKUh2Lda3b9CyNxriv"
         |        ],
         |        "fee" : 0
         |      }
         |    ],
         |   "parentId" : "CzrMgZskNAwoEE1F65yh7i8WMbYSW6bN3HMRbLKGQ5re"
         |}
         """.stripMargin)
    parser.parse(block.utf8String) match {
      case Right(blockJson) =>
        walletManagerRef ! s"new block added: $blockJson"
        Thread.sleep(1000)
        val walletBoxes: MMap[String, MMap[String, Json]] = Await.result((walletManagerRef ? GetWallet)
          .mapTo[MMap[String, MMap[String, Json]]], 10.seconds)
        val pk1Boxes: Option[MMap[String, Json]] = walletBoxes.get(pk1.toString)
        pk1Boxes match {
          case Some(map) =>
            assert(map.size == 3)
            assert(map.contains("GGDsEQdd5cnbgjKkac9HLpp2joGo6bWgmS2KvhJgd8b8"))
            map.get("GgNqzkSywewv10vCrb99UakEw1Myn5mqYXo3N4a6PWVW") match {
              case Some(json) => assert((json \\ "type").head.toString() == "\"Poly\"")
              case None => sys.error ("poly box was not found!")
            }
          case None => sys.error(s"no mapping for given public key: ${pk1.toString}")
        }
      case Left(e) => sys.error(s"Could not parse json $e")
    }
  }

}

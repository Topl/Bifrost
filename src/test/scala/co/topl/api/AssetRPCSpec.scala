package co.topl.api

import akka.actor.{ ActorRef, PoisonPill }
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{ HttpEntity, HttpMethods, HttpRequest, MediaTypes }
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.pattern.ask
import akka.util.{ ByteString, Timeout }
import co.topl.BifrostGenerators
import co.topl.crypto.{ PrivateKey25519, Signature25519 }
import co.topl.nodeView.history.History
import co.topl.http.api.routes.{ AssetApiRoute, WalletApiRoute }
import co.topl.nodeView.mempool.MemPool
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.{ AssetCreation, Transaction }
import co.topl.nodeView.box.proposition.PublicKey25519Proposition
import co.topl.nodeView.box.{ ArbitBox, AssetBox }
import co.topl.modifier.transaction.AssetCreation
import co.topl.nodeView.GenericNodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import co.topl.nodeView.{ CurrentView, NodeViewHolderRef }
import co.topl.settings.AppContext
import co.topl.nodeView.state.State
import co.topl.wallet.Wallet
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax._
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.reflect.io.Path
import scala.util.Try
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
  * Created by cykoz on 7/3/2017.
  */
class AssetRPCSpec extends AnyWordSpec
  with Matchers
  with ScalatestRouteTest
  with BifrostGenerators {

  val path: Path = Path("/tmp/bifrost/test-data")
  Try(path.deleteRecursively())

  /* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- */
  // save environment into a variable for reference throughout the application
  protected val appContext = new AppContext(settings, None)

  // Create Bifrost singleton actors
  private val nodeViewHolderRef: ActorRef = NodeViewHolderRef("nodeViewHolder", settings, appContext)
  /* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- */

  // setup route for testing
  val route: Route = AssetApiRoute(settings.restApi, nodeViewHolderRef).route
  val walletRoute: Route = WalletApiRoute(settings.restApi, nodeViewHolderRef).route

  def httpPOST(jsonRequest: ByteString): HttpRequest = {
    HttpRequest(
      HttpMethods.POST,
      uri = "/asset/",
      entity = HttpEntity(MediaTypes.`application/json`, jsonRequest)
    ).withHeaders(RawHeader("x-api-key", "test_key"))
  }

  def walletHttpPOST(jsonRequest: ByteString): HttpRequest = {
    HttpRequest(
      HttpMethods.POST,
      uri = "/wallet/",
      entity = HttpEntity(MediaTypes.`application/json`, jsonRequest)
    ).withHeaders(RawHeader("x-api-key", "test_key"))
  }

  implicit val timeout: Timeout = Timeout(10.seconds)

  private def actOnCurrentView(v: CurrentView[History, State, Wallet, MemPool]): CurrentView[History, State, Wallet, MemPool] = v

  private def view() = Await.result(
    (nodeViewHolderRef ? GetDataFromCurrentView(actOnCurrentView)).mapTo[CurrentView[History, State, Wallet, MemPool]],
    10.seconds)

  val publicKeys = Map(
    "investor" -> "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ",
    "producer" -> "A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb",
    "hub" -> "F6ABtYMsJABDLH2aj7XVPwQr5mH7ycsCE4QGQrLeB3xU"
  )
  // Unlock Secrets
  val gw: Wallet = view().vault
  gw.unlockKeyFile(publicKeys("investor"), "genesis")
  gw.unlockKeyFile(publicKeys("producer"), "genesis")
  gw.unlockKeyFile(publicKeys("hub"), "genesis")

  var asset: Option[AssetBox] = None
  var tx: Json = "".asJson

  "Asset RPC" should {
    "Create some assets" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "1",
           |   "method": "createAssets",
           |   "params": [{
           |     "issuer": "${publicKeys("hub")}",
           |     "recipient": "${publicKeys("investor")}",
           |     "amount": 10,
           |     "assetCode": "etherAssets",
           |     "fee": 0,
           |     "data": ""
           |   }]
           |}
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").head.asObject.isDefined shouldBe true
        val txHash = ((res \\ "result").head \\ "txHash").head.asString.get
        val txHashId = ModifierId(Base58.decode(txHash).get)
        val txInstance: Transaction = view().pool.getById(txHashId).get
        asset = Option(txInstance.newBoxes.head.asInstanceOf[AssetBox])

        val history = view().history
        val tempBlock = Block(history.bestBlockId,
          System.currentTimeMillis(),
          ArbitBox(PublicKey25519Proposition(history.bestBlockId.hashBytes), 0L, 10000L),
          Signature25519(Array.fill(Curve25519.SignatureLength)(1: Byte)),
          Seq(txInstance),
          settings.forgingSettings.version
        )
        view().state.applyModifier(tempBlock)
        view().pool.remove(txInstance)
        //Dont need further checks here since the subsequent tests would fail if this one did
      }
    }

    "Create assets prototype" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "2",
           |   "method": "createAssetsPrototype",
           |   "params": [{
           |     "issuer": "${publicKeys("hub")}",
           |     "recipient": "${publicKeys("investor")}",
           |     "amount": 10,
           |     "assetCode": "etherAssets",
           |     "fee": 0,
           |     "data": ""
           |   }]
           |}
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        tx = ((res \\ "result").head \\ "formattedTx").head
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").head.asObject.isDefined shouldBe true
      }
    }

    "Sign createAssets Prototype transaction" in {
      val requestBody = ByteString(
        s"""
           |{
           |  "jsonrpc": "2.0",
           |  "id": "3",
           |  "method": "signTx",
           |  "params": [{
           |    "signingKeys": ["${publicKeys("hub")}"],
           |    "protoTx": $tx
           |  }]
           |}
          """.stripMargin)

      walletHttpPOST(requestBody) ~> walletRoute ~> check {
        val res = parse(responseAs[String]).right.get
        tx = (res \\ "result").head
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").head.asObject.isDefined shouldBe true
      }
    }

    "Broadcast createAssetsPrototype transaction" in {
      val secret = view().vault.secretByPublicImage(
        PublicKey25519Proposition(Base58.decode(publicKeys("hub")).get)).get
      val tempTx = tx.as[AssetCreation].right.get
      val sig = secret.sign(tempTx.messageToSign)
      val signedTx = tempTx.copy(signatures = Map(PublicKey25519Proposition(Base58.decode(publicKeys("hub")).get) -> sig))

      val requestBody = ByteString(
        s"""
           |{
           |  "jsonrpc": "2.0",
           |  "id": "1",
           |  "method": "broadcastTx",
           |  "params": [{
           |    "tx": ${signedTx.json}
           |  }]
           |}
        """.stripMargin)

      walletHttpPOST(requestBody) ~> walletRoute ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").head.asObject.isDefined shouldBe true
      }
    }

    "Transfer target asset prototype" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "1",
           |   "method": "transferTargetAssetsPrototype",
           |   "params": [{
           |     "sender": ["${Base58.encode(asset.get.proposition.pubKeyBytes)}"],
           |     "recipient": "${publicKeys("producer")}",
           |     "assetId": "${Base58.encode(asset.get.id)}",
           |     "amount": 1,
           |     "fee": 0,
           |     "data": ""
           |   }]
           |}
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        tx = ((res \\ "result").head \\ "formattedTx").head
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").head.asObject.isDefined shouldBe true
      }
    }

//    "Broadcast transferTargetAssetsPrototype" in {
//      val prop = (tx \\ "from").head.asArray.get.head.asArray.get.head.asString.get
//      val secret = view().vault.secretByPublicImage(PublicKey25519Proposition(Base58.decode(prop).get)).get
//      val tempTx = tx.as[AssetTransfer].right.get
//      val sig = PrivateKey25519Companion.sign(secret, tempTx.messageToSign)
//      val signedTx = tempTx.copy(signatures = Map(PublicKey25519Proposition(Base58.decode(publicKeys("hub")).get) -> sig))
//
//      val requestBody = ByteString(
//        s"""
//           |{
//           |  "jsonrpc": "2.0",
//           |  "id": "1",
//           |  "method": "broadcastTx",
//           |  "params": [{
//           |    "tx": ${signedTx.json}
//           |  }]
//           |}
//        """.stripMargin)
//
//      walletHttpPOST(requestBody) ~> walletRoute ~> check {
//        val res = parse(responseAs[String]).right.get
//        (res \\ "error").isEmpty shouldBe true
//        (res \\ "result").head.asObject.isDefined shouldBe true
//      }
//    }

    "Transfer a target asset" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "1",
           |   "method": "transferTargetAssets",
           |   "params": [{
           |     "sender": ["${Base58.encode(asset.get.proposition.pubKeyBytes)}"],
           |     "recipient": "${publicKeys("producer")}",
           |     "assetId": "${Base58.encode(asset.get.id)}",
           |     "amount": 1,
           |     "fee": 0,
           |     "data": ""
           |   }]
           |}
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").head.asObject.isDefined shouldBe true
      }
    }

    "Transfer some assets" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "1",
           |   "method": "transferAssets",
           |   "params": [{
           |     "issuer": "${publicKeys("hub")}",
           |      "sender": ["${publicKeys("investor")}", "${publicKeys("hub")}", "${publicKeys("producer")}"],
           |     "recipient": "${publicKeys("producer")}",
           |     "amount": 1,
           |     "assetCode": "etherAssets",
           |     "fee": 0,
           |     "data": ""
           |   }]
           |}
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").head.asObject.isDefined shouldBe true

        //Removing transaction from mempool so as not to affect ProgramRPC tests
        val txHash = ((res \\ "result").head \\ "txHash").head.asString.get
        val txHashId = ModifierId(Base58.decode(txHash).get)
        val txInstance: Transaction = view().pool.getById(txHashId).get
        view().pool.remove(txInstance)
      }
    }

    "Create transfer assets prototype" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "1",
           |   "method": "transferAssetsPrototype",
           |   "params": [{
           |     "issuer": "${publicKeys("hub")}",
           |     "sender": ["${publicKeys("investor")}", "${publicKeys("hub")}", "${publicKeys("producer")}"],
           |     "recipient": "${publicKeys("producer")}",
           |     "amount": 5,
           |     "assetCode": "etherAssets",
           |     "fee": 0,
           |     "data": ""
           |   }]
           |}
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").head.asObject.isDefined shouldBe true
      }
    }
  }

  override def afterAll() {
    view().pool.unconfirmed.clear
    nodeViewHolderRef ! PoisonPill
  }
}
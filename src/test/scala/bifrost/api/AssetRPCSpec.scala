package bifrost.api

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.model.{HttpEntity, HttpMethods, HttpRequest, MediaTypes}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.pattern.ask
import akka.util.{ByteString, Timeout}
import bifrost.api.http.AssetApiRoute
import bifrost.blocks.BifrostBlock
import bifrost.history.BifrostHistory
import bifrost.mempool.BifrostMemPool
import bifrost.scorexMod.GenericNodeViewHolder.{CurrentView, GetCurrentView}
import bifrost.state.BifrostState
import bifrost.transaction.BifrostTransaction
import bifrost.transaction.box.ArbitBox
import bifrost.wallet.BWallet
import bifrost.{BifrostGenerators, BifrostNodeViewHolder}
import io.circe.parser.parse
import org.scalatest.{Matchers, WordSpec}
import bifrost.transaction.box.proposition.PublicKey25519Proposition
import bifrost.transaction.proof.Signature25519
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.reflect.io.Path
import scala.util.Try

/**
  * Created by cykoz on 7/3/2017.
  */
class AssetRPCSpec extends WordSpec
  with Matchers
  with ScalatestRouteTest
  with BifrostGenerators {

  val path: Path = Path("/tmp/scorex/test-data")
  Try(path.deleteRecursively())

  val actorSystem = ActorSystem(settings.agentName)
  val nodeViewHolderRef: ActorRef = actorSystem.actorOf(Props(new BifrostNodeViewHolder(settings)))
  nodeViewHolderRef
  val route = AssetApiRoute(settings, nodeViewHolderRef).route

  def httpPOST(jsonRequest: ByteString): HttpRequest = {
    HttpRequest(
      HttpMethods.POST,
      uri = "/asset/",
      entity = HttpEntity(MediaTypes.`application/json`, jsonRequest)
    )
  }

  implicit val timeout = Timeout(10.seconds)

  private def view() = Await.result((nodeViewHolderRef ? GetCurrentView)
    .mapTo[CurrentView[BifrostHistory, BifrostState, BWallet, BifrostMemPool]], 10.seconds)

  val publicKeys = Map(
    "investor" -> "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ",
    "producer" -> "A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb",
    "hub" -> "F6ABtYMsJABDLH2aj7XVPwQr5mH7ycsCE4QGQrLeB3xU"
  )
  // Unlock Secrets
  val gw: BWallet = view().vault
  gw.unlockKeyFile(publicKeys("investor"), "genesis")
  gw.unlockKeyFile(publicKeys("producer"), "genesis")
  gw.unlockKeyFile(publicKeys("hub"), "genesis")

  // TODO asset redemption does not work
  "Asset RPC" should {
    "Redeem some assets" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "30",
           |   "method": "redeemAssets",
           |   "params": [{
           |     "signingPublicKey": "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ"
           |   }]
           |}
        """.stripMargin)
      //      httpPOST(requestBody) ~> route ~> check {
      //        val res = parse(responseAs[String]).right.get
      //        (res \\ "error").head.asObject.isDefined shouldBe true
      //        (res \\ "result").isEmpty shouldBe true
      //      }
    }


    "Create some assets" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "30",
           |   "method": "createAssets",
           |   "params": [{
           |     "issuer": "${publicKeys("hub")}",
           |     "to": "${publicKeys("investor")}",
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
        val txInstance: BifrostTransaction = view().pool.getById(Base58.decode(txHash).get).get

        //        val boxSC = BifrostStateChanges(txInstance.boxIdsToOpen.toSet,
        //          txInstance.newBoxes.toSet,
        //          System.currentTimeMillis())
        //
        //        view().state.applyChanges(boxSC, Ints.toByteArray(99)).get

        //To update wallet correctly gw.scanPersistent needs to be used to manually add a block
        // as opposed to creating a new state change like above
        val history = view().history
        val tempBlock = BifrostBlock(history.bestBlockId,
          System.currentTimeMillis(),
          ArbitBox(PublicKey25519Proposition(history.bestBlockId), 0L, 10000L),
          Signature25519(Array.fill(Curve25519.SignatureLength)(1: Byte)),
          Seq(txInstance),
          10L
        )
        gw.scanPersistent(tempBlock)
        //Dont need further checks here since the subsequent tests would fail if this one did
      }
    }

    "Transfer some assets" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "30",
           |   "method": "transferAssets",
           |   "params": [{
           |     "issuer": "${publicKeys("hub")}",
           |     "recipient": "${publicKeys("producer")}",
           |     "amount": 1,
           |     "assetCode": "etherAssets",
           |     "fee": 0,
           |     "publicKeysToSendFrom": [],
           |     "data": ""
           |   }]
           |}
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").head.asObject.isDefined shouldBe true

        //Removing transaction from mempool so as not to affect ContractRPC tests
        val txHash = ((res \\ "result").head \\ "txHash").head.asString.get
        val txInstance: BifrostTransaction = view().pool.getById(Base58.decode(txHash).get).get
        view().pool.remove(txInstance)
      }
    }

    "Transfer some assets from a specified public key in wallet" in {
      val requestBody = ByteString(
        s"""
           |{
           |
           |   "jsonrpc": "2.0",
           |   "id": "30",
           |   "method": "transferAssets",
           |   "params": [{
           |     "issuer": "${publicKeys("hub")}",
           |     "recipient": "${publicKeys("producer")}",
           |     "publicKeyToSendFrom": ["${publicKeys("investor")}", "${publicKeys("hub")}"],
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
        val txHash = ((res \\ "result").head \\ "txHash").head.asString.get
        val txInstance: BifrostTransaction = view().pool.getById(Base58.decode(txHash).get).get
        view().pool.remove(txInstance)
      }
    }

    "Transfer some asset from a specified public key and specify a change address" in {
      val requestBody = ByteString(
        s"""
           |{
           |
           |   "jsonrpc": "2.0",
           |   "id": "30",
           |   "method": "transferAssets",
           |   "params": [{
           |     "issuer": "${publicKeys("hub")}",
           |     "recipient": "${publicKeys("hub")}",
           |     "publicKeyToSendFrom": ["${publicKeys("investor")}"],
           |     "publicKeyToSendChangeTo": "${publicKeys("producer")}",
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
        val txHash = ((res \\ "result").head \\ "txHash").head.asString.get
        val txInstance: BifrostTransaction = view().pool.getById(Base58.decode(txHash).get).get
        view().pool.remove(txInstance)
      }
    }

    "Transfer some assets and specify a change address but no sender" in {
      val requestBody = ByteString(
        s"""
           |{
           |
           |   "jsonrpc": "2.0",
           |   "id": "30",
           |   "method": "transferAssets",
           |   "params": [{
           |     "issuer": "${publicKeys("hub")}",
           |     "recipient": "${publicKeys("hub")}",
           |     "publicKeyToSendChangeTo": "${publicKeys("producer")}",
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
        val txHash = ((res \\ "result").head \\ "txHash").head.asString.get
        val txInstance: BifrostTransaction = view().pool.getById(Base58.decode(txHash).get).get
        view().pool.remove(txInstance)
      }
    }
  }

  object AssetRPCSpec {
    val path: Path = Path("/tmp/scorex/test-data")
    Try(path.deleteRecursively())
  }
}
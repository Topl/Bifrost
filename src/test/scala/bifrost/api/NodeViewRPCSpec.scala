package bifrost.api

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.model.{HttpEntity, HttpMethods, HttpRequest, MediaTypes}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.pattern.ask
import akka.util.{ByteString, Timeout}
import bifrost.api.http.{AssetApiRoute, NodeViewApiRoute}
import bifrost.history.BifrostHistory
import bifrost.mempool.BifrostMemPool
import bifrost.scorexMod.GenericNodeViewHolder.{CurrentView, GetCurrentView}
import bifrost.state.BifrostState
import bifrost.wallet.BWallet
import bifrost.{BifrostGenerators, BifrostNodeViewHolder}
import io.circe.Json
import io.circe.parser.parse
import org.scalatest.{Matchers, WordSpec}
import bifrost.block.Block
import bifrost.blocks.BifrostBlock
import bifrost.transaction.bifrostTransaction.BifrostTransaction
import bifrost.transaction.box.ArbitBox
import bifrost.transaction.box.proposition.PublicKey25519Proposition
import bifrost.transaction.proof.Signature25519
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519

import scala.concurrent.Await
import scala.concurrent.duration._

class NodeViewRPCSpec extends WordSpec
  with Matchers
  with ScalatestRouteTest
  with BifrostGenerators {

  val actorSystem = ActorSystem(settings.agentName)
  val nodeViewHolderRef: ActorRef = actorSystem.actorOf(Props(new BifrostNodeViewHolder(settings)))
  nodeViewHolderRef
  val route = NodeViewApiRoute(settings, nodeViewHolderRef).route

  val routeAsset = AssetApiRoute(settings, nodeViewHolderRef).route

  def httpPOST(jsonRequest: ByteString): HttpRequest = {
    HttpRequest(
      HttpMethods.POST,
      uri = "/nodeView/",
      entity = HttpEntity(MediaTypes.`application/json`, jsonRequest)
    )
  }

  def httpPOSTAsset(jsonRequest: ByteString): HttpRequest = {
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

  var txHash: String = ""
  var assetTxHash: String = ""
  var blockId: Block.BlockId = Array[Byte]()

  val requestBody = ByteString(
    s"""
       |{
       |   "jsonrpc": "2.0",
       |   "id": "30",
       |   "method": "createAssets",
       |   "params": [{
       |     "issuer": "${publicKeys("hub")}",
       |     "recipient": "${publicKeys("investor")}",
       |     "amount": 10,
       |     "assetCode": "x",
       |     "fee": 0,
       |     "data": ""
       |   }]
       |}
        """.stripMargin)

  httpPOSTAsset(requestBody) ~> routeAsset ~> check {
    val res = parse(responseAs[String]).right.get
    (res \\ "error").isEmpty shouldBe true
    (res \\ "result").head.asObject.isDefined shouldBe true
    assetTxHash = ((res \\ "result").head \\ "txHash").head.asString.get
  }

  "NodeView RPC" should {
    "Get mempool" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "30",
           |   "method": "pool",
           |   "params": [{}]
           |}
          """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").isInstanceOf[List[Json]] shouldBe true
        var txHashesArray = ((res \\ "result").head \\ "txHash")
        txHashesArray.find(tx => tx.asString.get == assetTxHash) match {
          case Some (tx) =>
            txHash = tx.asString.get
          case None =>
        }
        txHash shouldEqual assetTxHash
        val txInstance: BifrostTransaction = view().pool.getById(Base58.decode(txHash).get).get
        val history = view().history
        //Create a block with the above created createAssets transaction
        val tempBlock = BifrostBlock(history.bestBlockId,
          System.currentTimeMillis(),
          ArbitBox(PublicKey25519Proposition(history.bestBlockId), 0L, 10000L),
          Signature25519(Array.fill(Curve25519.SignatureLength)(1: Byte)),
          Seq(txInstance),
          10L
        )
        history.append(tempBlock)
        blockId = tempBlock.id
        //Removing the createAssets transaction from the mempool
        view().pool.remove(txInstance)
      }
    }

    "Get transaction by id" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "30",
           |   "method": "transactionById",
           |   "params": [{
           |      "transactionId": "${txHash}"
           |   }]
           |}
           |
          """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").isInstanceOf[List[Json]] shouldBe true
        ((res \\ "result").head \\ "txHash").head.asString.get shouldEqual txHash
      }
    }

    "Get block by id" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |
           |   "id": "30",
           |   "method": "persistentModifierById",
           |   "params": [{
           |      "modifierId": "${Base58.encode(blockId)}"
           |   }]
           |}
           |
          """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").isInstanceOf[List[Json]] shouldBe true
        val txsArray = ((res \\ "result").head \\ "txs").head.asArray.get
        txsArray.filter(tx => {tx \\"txHash"} == txHash)
        //Checking that the block found contains the above createAssets transaction
        //since that block's id was used as the search parameter
        txsArray.size shouldEqual 1
      }
    }
  }
}

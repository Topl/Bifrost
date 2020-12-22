package co.topl.api

import akka.http.scaladsl.server.Route
import akka.util.ByteString
import co.topl.crypto.Signature25519
import co.topl.http.api.routes.NodeViewApiRoute
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.{AssetCreation, Transaction}
import co.topl.nodeView.state.box.ArbitBox
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import io.circe.Json
import io.circe.parser.parse
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scorex.crypto.signatures.{Curve25519, PublicKey, Signature}

import scala.reflect.io.Path
import scala.util.Try

class NodeViewRPCSpec extends AnyWordSpec
  with Matchers
  with RPCMockState {

  // setup route for testing
  val route: Route = NodeViewApiRoute(settings.restApi, nodeViewHolderRef).route

  val tx: AssetCreation = assetCreationGen.sample.get
  var txHash: String = ""
  var assetTxHash: String = tx.id.toString
  var assetTxInstance: Transaction = _
  var blockId: Block.BlockId = _

  view().pool.put(tx)

  "NodeView RPC" should {
    "Get first 100 transactions in mempool" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "1",
           |   "method": "mempool",
           |   "params": [{}]
           |}
          """.stripMargin)

      httpPOST("/nodeView/", requestBody) ~> route ~> check {
        val res: Json = parse(responseAs[String]) match {case Right(re) => re; case Left(ex) => throw ex}
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").isInstanceOf[List[Json]] shouldBe true
        val txHashesArray = (res \\ "result").head \\ "txHash"
        txHashesArray.find(tx => tx.asString.get == assetTxHash) match {
          case Some (tx) =>
            txHash = tx.asString.get
          case None =>
        }
        txHash shouldEqual assetTxHash
        assert(txHashesArray.size <= 100)
        val txHashId = ModifierId(txHash)
        assetTxInstance = view().pool.modifierById(txHashId).get
        val history = view().history
        //Create a block with the above created createAssets transaction
        val tempBlock = Block(history.bestBlockId,
          System.currentTimeMillis(),
          ArbitBox(PublicKey25519Proposition(PublicKey @@ history.bestBlockId.hashBytes), 0L, 10000L),
          Signature25519(Signature @@ Array.fill(Curve25519.SignatureLength)(1: Byte)),
          Seq(assetTxInstance),
          settings.application.version.blockByte
        )
        history.storage.update(tempBlock, 0, isBest = false)
        blockId = tempBlock.id
      }
    }

    "Get transaction from the mempool by id" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "1",
           |   "method": "transactionFromMempool",
           |   "params": [{
           |      "transactionId": "$txHash"
           |   }]
           |}
           |
          """.stripMargin)

      httpPOST("/nodeView/", requestBody) ~> route ~> check {
        val res: Json = parse(responseAs[String]) match {case Right(re) => re; case Left(ex) => throw ex}
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").isInstanceOf[List[Json]] shouldBe true
        ((res \\ "result").head \\ "txHash").head.asString.get shouldEqual txHash

        //Removing the createAssets transaction from the mempool
        view().pool.remove(assetTxInstance)
      }
    }

    "Get a confirmed transaction by id" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "1",
           |   "method": "transactionById",
           |   "params": [{
           |      "transactionId": "$txHash"
           |   }]
           |}
           |
          """.stripMargin)

      httpPOST("/nodeView/", requestBody) ~> route ~> check {
        val res: Json = parse(responseAs[String]) match {case Right(re) => re; case Left(ex) => throw ex}
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
           |   "id": "1",
           |   "method": "blockById",
           |   "params": [{
           |      "blockId": "$blockId"
           |   }]
           |}
           |
          """.stripMargin)

      httpPOST("/nodeView/", requestBody) ~> route ~> check {
        val res: Json = parse(responseAs[String]) match {case Right(re) => re; case Left(ex) => throw ex}
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").isInstanceOf[List[Json]] shouldBe true
        val txsArray = ((res \\ "result").head \\ "txs").head.asArray.get
        txsArray.filter(tx => {(tx \\"txHash").head.asString.get == txHash})
        //Checking that the block found contains the above createAssets transaction
        //since that block's id was used as the search parameter
        txsArray.size shouldEqual 1
      }
    }
  }
}


object NodeViewRPCSpec {
  val path: Path = Path("/tmp/bifrost/test-data")
  Try(path.deleteRecursively())
}

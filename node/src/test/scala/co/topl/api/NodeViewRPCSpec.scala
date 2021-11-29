package co.topl.api

import akka.util.ByteString
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.Transaction.TX
import co.topl.nodeView.TestableNodeViewHolder
import co.topl.nodeView.history.History
import co.topl.utils.GeneratorOps.GeneratorOps
import io.circe.Json
import io.circe.parser.parse
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class NodeViewRPCSpec extends AnyWordSpec with Matchers with RPCMockState with EitherValues {

  var txs: Seq[TX] = _
  var txId: String = _
  var block: Block = _

  override def beforeAll(): Unit = {
    super.beforeAll()

    txs = bifrostTransactionSeqGen.sampleFirst()
    txId = txs.head.id.toString
    block = blockCurve25519Gen.sampleFirst().copy(transactions = txs)

    import akka.actor.typed.scaladsl.adapter._
    TestableNodeViewHolder.setNodeView(
      nodeViewHolderRef,
      current =>
        current.copy(history = current.history match {
          case h: History =>
            h.storage.update(block, isBest = true)
            h
        })
    )(system.toTyped)

    view().mempool.putWithoutCheck(txs, block.timestamp)
  }

  "NodeView RPC" should {
    "Get current head of the chain" in {
      val requestBody = ByteString(s"""
        |{
        |   "jsonrpc": "2.0",
        |   "id": "1",
        |   "method": "topl_head",
        |   "params": [{}]
        |}
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res: Json = parse(responseAs[String]).value
        val head = res.hcursor.downField("result").as[Json].toString
        head should include("bestBlock")
        res.hcursor.downField("error").values shouldBe None
      }
    }

    "Get info about current head of the chain" in {
      val requestBody = ByteString(s"""
        |{
        |   "jsonrpc": "2.0",
        |   "id": "1",
        |   "method": "topl_headInfo",
        |   "params": [{}]
        |}
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res: Json = parse(responseAs[String]).value
        val headInfo = res.hcursor.downField("result").as[Json].toString
        headInfo should include("bestBlockId")
        res.hcursor.downField("error").values shouldBe None
      }
    }

    "Get first 100 transactions in mempool" in {
      val requestBody = ByteString(s"""
        |{
        |   "jsonrpc": "2.0",
        |   "id": "1",
        |   "method": "topl_mempool",
        |   "params": [{}]
        |}
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res: Json = parse(responseAs[String]) match { case Right(re) => re; case Left(ex) => throw ex }
        val txIds = (res \\ "result").head.asArray.get.flatMap(txJson => (txJson \\ "txId").head.asString)
        txs.foreach(tx => txIds.contains(tx.id.toString))
      }
    }

    "Get transaction from the mempool by id" in {
      val requestBody = ByteString(s"""
        |{
        |   "jsonrpc": "2.0",
        |   "id": "1",
        |   "method": "topl_transactionFromMempool",
        |   "params": [{
        |      "transactionId": "$txId"
        |   }]
        |}
        |
        """.stripMargin)

      view().mempool.putWithoutCheck(Seq(txs.head), block.timestamp)

      httpPOST(requestBody) ~> route ~> check {
        val res: Json = parse(responseAs[String]) match { case Right(re) => re; case Left(ex) => throw ex }
        ((res \\ "result").head \\ "txId").head.asString.get shouldEqual txId
      }
      view().mempool.remove(txs.head)
    }

    "Return correct error response when an id with non-base58 character is used for querying transactions" in {
      val invalidCharId: String = "=" ++ txId.tail
      val modifierQueryMethods = Seq("topl_transactionById", "topl_transactionFromMempool", "topl_blockById")
      val idTypes = Seq("transactionId", "transactionId", "blockId")
      def requestBody(idType: String, rpcMethod: String, txId: String): ByteString = ByteString(s"""
        |{
        |   "jsonrpc": "2.0",
        |   "id": "1",
        |   "method": "$rpcMethod",
        |   "params": [{
        |      "$idType": "$txId"
        |   }]
        |}
        |
        """.stripMargin)

      idTypes.zip(modifierQueryMethods).map { case (idType, rpcMethod) =>
        httpPOST(requestBody(idType, rpcMethod, invalidCharId)) ~> route ~> check {
          val res: String = parse(responseAs[String]).value.hcursor.downField("error").as[Json].toString
          res should include("Value is not Base 58")
        }
      }
    }

    "Return correct error response when an id with incorrect size is used for querying transactions" in {
      val invalidLengthId: String = txId.tail
      val modifierQueryMethods = Seq("topl_transactionById", "topl_transactionFromMempool", "topl_blockById")
      val idTypes = Seq("transactionId", "transactionId", "blockId")
      def requestBody(idType: String, rpcMethod: String, txId: String): ByteString = ByteString(s"""
        |{
        |   "jsonrpc": "2.0",
        |   "id": "1",
        |   "method": "$rpcMethod",
        |   "params": [{
        |      "$idType": "$txId"
        |   }]
        |}
        |
        """.stripMargin)

      idTypes.zip(modifierQueryMethods).map { case (idType, rpcMethod) =>
        httpPOST(requestBody(idType, rpcMethod, invalidLengthId)) ~> route ~> check {
          val res: String = parse(responseAs[String]).value.hcursor.downField("error").as[Json].toString
          res should include("Invalid size for ModifierId")
        }
      }
    }

    "Get a confirmed transaction by id" in {

      val requestBody = ByteString(s"""
        |{
        |   "jsonrpc": "2.0",
        |   "id": "1",
        |   "method": "topl_transactionById",
        |   "params": [{
        |      "transactionId": "$txId"
        |   }]
        |}
        |
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res: Json = parse(responseAs[String]) match { case Right(re) => re; case Left(ex) => throw ex }
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").isInstanceOf[List[Json]] shouldBe true
        ((res \\ "result").head \\ "txId").head.asString.get shouldEqual txId
      }
    }

    "Get block by id" in {
      val requestBody = ByteString(s"""
        |{
        |   "jsonrpc": "2.0",
        |
        |   "id": "1",
        |   "method": "topl_blockById",
        |   "params": [{
        |      "blockId": "${block.id}"
        |   }]
        |}
        |
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res: Json = parse(responseAs[String]) match { case Right(re) => re; case Left(ex) => throw ex }
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").isInstanceOf[List[Json]] shouldBe true
      }
    }

    "Get a segment of the chain by height range" in {
      val requestBody = ByteString(s"""
        |{
        |   "jsonrpc": "2.0",
        |   "id": "1",
        |   "method": "topl_blocksInRange",
        |   "params": [{
        |      "startHeight": 1,
        |      "endHeight": 1
        |    }]
        |}
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res: Json = parse(responseAs[String]).value
        val blocks = res.hcursor.downField("result").as[Json].toString
        blocks should include("\"height\" : 1")
        res.hcursor.downField("error").values shouldBe None
      }
    }

    "Fail if an invalid height range is provided for blocksInRange" in {
      val ranges = Seq((0, 1), (2, 1), (2, 2))
      def requestBody(startHeight: Long, endHeight: Long): ByteString = ByteString(s"""
        |{
        |   "jsonrpc": "2.0",
        |   "id": "1",
        |   "method": "topl_blocksInRange",
        |   "params": [{
        |      "startHeight": $startHeight,
        |      "endHeight": $endHeight
        |    }]
        |}
        """.stripMargin)

      ranges.map { case (startHeight, endHeight) =>
        httpPOST(requestBody(startHeight, endHeight)) ~> route ~> check {
          val res: String = parse(responseAs[String]).value.hcursor.downField("error").as[Json].toString
          res should include("Invalid height range")
        }
      }
    }

    "Return info about the node" in {
      val requestBody = ByteString(s"""
        |{
        |   "jsonrpc": "2.0",
        |   "id": "1",
        |   "method": "topl_info",
        |   "params": [{}]
        |}
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res: Json = parse(responseAs[String]).value
        val info = res.hcursor.downField("result").get[String]("network").value
        val version = res.hcursor.downField("result").get[String]("version").value
        info shouldEqual appContext.networkType.toString
        version shouldEqual settings.application.version.toString
        res.hcursor.downField("error").values shouldBe None
      }
    }

    "Return the forging status status of the node" in {
      val requestBody = ByteString(s"""
        |{
        |   "jsonrpc": "2.0",
        |   "id": "1",
        |   "method": "topl_nodeStatus",
        |   "params": [{}]
        |}
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res: Json = parse(responseAs[String]).value
        val forgingStatus = res.hcursor.downField("result").get[String]("forgingStatus").value
        forgingStatus shouldEqual "active"
        res.hcursor.downField("error").values shouldBe None
      }
    }
  }
}

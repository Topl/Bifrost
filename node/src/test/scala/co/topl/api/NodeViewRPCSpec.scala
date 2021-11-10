package co.topl.api

import cats.implicits._
import co.topl.utils.catsInstances._
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
    txId = txs.head.id.show
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
        txs.foreach(tx => txIds.contains(tx.id.show))
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
          res should include("failed to decode base-58 string")
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
          res should include("failed to decode modifier ID")
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
        |      "blockId": "${block.id.show}"
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
  }
}

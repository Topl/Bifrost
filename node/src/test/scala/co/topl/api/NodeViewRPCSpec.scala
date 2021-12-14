package co.topl.api

import akka.actor.typed.scaladsl.adapter.ClassicActorSystemOps
import akka.util.ByteString
import co.topl.consensus.{blockVersion, getProtocolRules, ActorForgerInterface}
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.Transaction.TX
import co.topl.nodeView.TestableNodeViewHolder
import co.topl.nodeView.history.History
import co.topl.utils.GeneratorOps.GeneratorOps
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax._
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

    "Get balances for given addresses" in {
      val params: Json = Map("addresses" -> keyRingCurve25519.addresses.map(_.asJson).toList).asJson
      val requestBody = ByteString(s"""
        |{
        |   "jsonrpc": "2.0",
        |   "id": "1",
        |   "method": "topl_balances",
        |   "params": [$params]
        |}
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        println(keyRingCurve25519.addresses)
        val res: Json = parse(responseAs[String]).value
        val balances = res.hcursor.downField("result").as[Json].value
        keyRingCurve25519.addresses.map { addr =>
          balances.toString() should include(addr.toString)
        }
        keyRingCurve25519.addresses.map { addr =>
          balances.hcursor.downField(addr.toString).get[Json]("Balances").map { balance =>
            val testnetBalance = settings.forging.privateTestnet.map(_.testnetBalance).get.toString
            balance.hcursor.downField("Polys").as[String].value shouldEqual testnetBalance
            balance.hcursor.downField("Arbits").as[String].value shouldEqual testnetBalance
          }
        }
        res.hcursor.downField("error").values shouldBe None
      }
    }

    "Get first 100 transactions in mempool" in {
      val aliases = Seq("topl_getPendingTransactions", "topl_mempool")
      def requestBody(methodName: String): ByteString = ByteString(s"""
        |{
        |   "jsonrpc": "2.0",
        |   "id": "1",
        |   "method": "$methodName",
        |   "params": [{}]
        |}
        """.stripMargin)

      aliases.map { alias =>
        httpPOST(requestBody(alias)) ~> route ~> check {
          val res: Json = parse(responseAs[String]).value
          val txIds =
            res.hcursor.downField("result").as[Seq[Json]].value.map(_.hcursor.downField("txId").as[String].value)
          txs.foreach(tx => txIds.contains(tx.id.toString) shouldBe true)
          res.hcursor.downField("error").values shouldBe None
        }
      }
    }

    "Get transaction from the mempool by id" in {
      val aliases = Seq("topl_getPendingTransactionById", "topl_transactionFromMempool")
      def requestBody(methodName: String): ByteString = ByteString(s"""
        |{
        |   "jsonrpc": "2.0",
        |   "id": "1",
        |   "method": "$methodName",
        |   "params": [{
        |      "transactionId": "$txId"
        |   }]
        |}
        |
        """.stripMargin)

      view().mempool.putWithoutCheck(Seq(txs.head), block.timestamp)

      aliases.map { alias =>
        httpPOST(requestBody(alias)) ~> route ~> check {
          val res: Json = parse(responseAs[String]).value
          res.hcursor.downField("result").get[String]("txId").value shouldEqual txId
          res.hcursor.downField("error").values shouldBe None
        }
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
        val res: Json = parse(responseAs[String]).value
        res.hcursor.downField("result").as[Json].toString should include(txId)
        res.hcursor.downField("error").values shouldBe None
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
        val res: Json = parse(responseAs[String]).value
        res.hcursor.downField("result").as[Json].toString should include(block.id.toString)
        res.hcursor.downField("error").values shouldBe None
      }
    }

    "Get multiple block by ids" in {
      val requestBody = ByteString(s"""
        |{
        |   "jsonrpc": "2.0",
        |
        |   "id": "1",
        |   "method": "topl_blocksByIds",
        |   "params": [{
        |      "blockIds": ["${block.id}"]
        |   }]
        |}
        |
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res: Json = parse(responseAs[String]).value
        res.hcursor.downField("result").as[Json].toString should include(block.id.toString)
        res.hcursor.downField("error").values shouldBe None
      }
    }

    "Fail if one of the ids are invalid" in {
      val requestBody = ByteString(s"""
        |{
        |   "jsonrpc": "2.0",
        |
        |   "id": "1",
        |   "method": "topl_blocksByIds",
        |   "params": [{
        |      "blockIds": ["${block.id}", "${blockCurve25519Gen.sampleFirst().id}"]
        |   }]
        |}
        |
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res: Json = parse(responseAs[String]).value
        res.hcursor.downField("error").as[Json].toString should include("No corresponding block found for the given id")
      }
    }

    "Get block at the height given" in {
      val requestBody = ByteString(s"""
        |{
        |   "jsonrpc": "2.0",
        |   "id": "1",
        |   "method": "topl_blockByHeight",
        |   "params": [{
        |      "height": 1
        |    }]
        |}
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res: Json = parse(responseAs[String]).value
        val blockId = res.hcursor.downField("result").downField("header").get[String]("id").value
        blockId shouldEqual block.id.toString
        res.hcursor.downField("error").values shouldBe None
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
        val blocks = res.hcursor.downField("result").as[Json].value.toString
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
          val res: String = parse(responseAs[String]).value.hcursor.downField("error").as[Json].value.toString
          res should include("Invalid height range")
        }
      }
    }

    "Get block ids of a segment of the chain by height range" in {
      val requestBody = ByteString(s"""
        |{
        |   "jsonrpc": "2.0",
        |   "id": "1",
        |   "method": "topl_blockIdsInRange",
        |   "params": [{
        |      "startHeight": 1,
        |      "endHeight": 1
        |    }]
        |}
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res: Json = parse(responseAs[String]).value
        val blockIds = res.hcursor.downField("result").as[Seq[String]].value
        blockIds.head shouldEqual block.id.toString
        res.hcursor.downField("error").values shouldBe None
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
        val res: Json = parse(responseAs[String]).value.hcursor.downField("result").as[Json].value
        res.hcursor.get[String]("network").value shouldEqual appContext.networkType.toString
        res.hcursor.get[String]("appVersion").value shouldEqual settings.application.version.toString
        res.hcursor.get[String]("protocolVersion").value shouldEqual getProtocolRules(
          view().history.height
        ).version.toString
        res.hcursor.get[String]("blockVersion").value shouldEqual blockVersion(view().history.height).toString
        res.hcursor.downField("error").values shouldBe None
      }
    }

    "Return the forging status status of the node" in {
      val requestBody = ByteString(s"""
        |{
        |   "jsonrpc": "2.0",
        |   "id": "1",
        |   "method": "topl_status",
        |   "params": [{}]
        |}
        """.stripMargin)

      implicit val typedSystem: akka.actor.typed.ActorSystem[_] = system.toTyped
      val forgerInterface = new ActorForgerInterface(forgerRef)

      def nodeStatus(): String =
        httpPOST(requestBody) ~> route ~> check {
          val res: Json = parse(responseAs[String]).value
          val forgingStatus = res.hcursor.downField("result").get[String]("forgingStatus").value
          val mempoolSize = res.hcursor.downField("result").get[Int]("numberOfPendingTransactions").value
          mempoolSize shouldEqual view().mempool.size
          res.hcursor.downField("error").values shouldBe None
          forgingStatus
        }

      forgerInterface.stopForging()
      nodeStatus() shouldEqual "idle"
      forgerInterface.startForging()
      nodeStatus() shouldEqual "active"
    }

    "Return the confirmation status of a confirmed, unconfirmed transactions" in {
      val unconfirmedTx = bifrostTransactionSeqGen.sampleFirst()
      val unconfirmedTxId = unconfirmedTx.head.id.toString
      val requestBody = ByteString(s"""
        |{
        |   "jsonrpc": "2.0",
        |
        |   "id": "1",
        |   "method": "topl_confirmationStatus",
        |   "params": [{
        |      "transactionIds": ["$txId", "$unconfirmedTxId"]
        |   }]
        |}
        |
        """.stripMargin)

      view().mempool.putWithoutCheck(Seq(unconfirmedTx.head), block.timestamp)

      httpPOST(requestBody) ~> route ~> check {
        val res: Json = parse(responseAs[String]).value
        val confirmedStatus = res.hcursor.downField("result").get[Json](txId).value
        val unconfirmedStatus = res.hcursor.downField("result").get[Json](unconfirmedTxId).value
        confirmedStatus.hcursor.downField("status").as[String].value shouldEqual "Confirmed"
        confirmedStatus.hcursor.downField("depthFromHead").as[Int].value shouldEqual 0
        unconfirmedStatus.hcursor.downField("status").as[String].value shouldEqual "Unconfirmed"
        unconfirmedStatus.hcursor.downField("depthFromHead").as[Int].value shouldEqual -1
        res.hcursor.downField("error").values shouldBe None
      }

      view().mempool.remove(unconfirmedTx.head)
    }

    "Return an error when a transaction cannot be found" in {
      val tx = bifrostTransactionSeqGen.sampleFirst()
      val txId = tx.head.id.toString
      val requestBody = ByteString(s"""
                                      |{
                                      |   "jsonrpc": "2.0",
                                      |
                                      |   "id": "1",
                                      |   "method": "topl_confirmationStatus",
                                      |   "params": [{
                                      |      "transactionIds": ["$txId", "$txId"]
                                      |   }]
                                      |}
                                      |
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res: String = parse(responseAs[String]).value.hcursor.downField("error").as[Json].toString
        res should include("Could not find one or more of the specified transactions")
      }
    }
  }
}

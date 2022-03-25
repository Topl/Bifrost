package co.topl.api

import akka.util.ByteString
import cats.data.NonEmptyChain
import cats.implicits._
import co.topl.codecs._
import co.topl.consensus.TestableConsensusViewHolder
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.Transaction.TX
import co.topl.nodeView.TestableNodeViewHolder
import co.topl.nodeView.history.History
import co.topl.nodeView.state.State
import co.topl.utils.GeneratorOps.GeneratorOps
import co.topl.utils.Int128
import co.topl.utils.implicits._
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.{EitherValues, OptionValues}

class NodeViewRPCSpec extends AnyWordSpec with Matchers with RPCMockState with EitherValues with OptionValues {

  var blocksAndTx: NonEmptyChain[(Block, Seq[TX])] = _
  var txId: String = _
  var blocks: NonEmptyChain[Block] = _

  override def beforeAll(): Unit = {
    super.beforeAll()

    blocks = blockchainGen(10).sampleFirst()
    blocksAndTx = blocks.map(b => b -> b.transactions)
    txId = blocksAndTx.last._2.head.id.show

    import akka.actor.typed.scaladsl.adapter._

    TestableNodeViewHolder.setNodeView(
      nodeViewHolderRef,
      current =>
        current.copy(
          history = current.history match {
            case h: History => blocks.foldLeft(h)((accHistory, block) => accHistory.append(block, Seq()).get._1)
          },
          state = current.state match {
            case s: State => blocks.foldLeft(s)((accState, block) => accState.applyModifier(block).get)
          }
        )
    )(system.toTyped)

    TestableConsensusViewHolder.updateConsensusView(
      consensusHolderRef,
      blocks.last
    )(system.toTyped)

    blocksAndTx.map { case (block, txs) =>
      view().mempool.putWithoutCheck(txs, block.timestamp)
    }
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
        val res1: String = responseAs[String]
        val res: Json = parse(res1).value
        val balances = res.hcursor.downField("result").as[Json].value
        keyRingCurve25519.addresses.map { addr =>
          balances.toString() should include(addr.toString)
        }
        keyRingCurve25519.addresses.map { addr =>
          balances.hcursor.downField(addr.toString).get[Json]("Balances").map { balance =>
            val testnetBalance = settings.application.genesis.generated.map(_.balanceForEachParticipant).get
            balance.hcursor.downField("Polys").as[Int128].value shouldEqual testnetBalance
            balance.hcursor.downField("Arbits").as[Int128].value shouldEqual testnetBalance
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
          val txIdsRes =
            res.hcursor.downField("result").as[Seq[Json]].value.map(_.hcursor.downField("txId").as[ModifierId].value)

          // All the transactions from the generated chain should exist in the response
          blocksAndTx.map { case (_, txs) => txs.foreach(tx => txIdsRes.contains(tx.id) shouldBe true) }
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

      aliases.map { alias =>
        httpPOST(requestBody(alias)) ~> route ~> check {
          val res: Json = parse(responseAs[String]).value
          res.hcursor.downField("result").get[String]("txId").value shouldEqual txId
          res.hcursor.downField("error").values shouldBe None
        }
      }
    }

    def modifierRequestBody(idType: String, rpcMethod: String, id: String): ByteString = ByteString(s"""
        |{
        |   "jsonrpc": "2.0",
        |   "id": "1",
        |   "method": "$rpcMethod",
        |   "params": [{
        |      "$idType": "$id"
        |   }]
        |}
        |
        """.stripMargin)

    "Return correct error response when a block id is provided for transactionById" in {
      httpPOST(
        modifierRequestBody("transactionId", "topl_transactionById", blocks.last.id.toString)
      ) ~> route ~> check {
        val res: String = parse(responseAs[String]).value.hcursor.downField("error").as[Json].toString
        res should include("The requested id's type is not an id type for Transaction")
      }
    }

    "Return correct error response when a block id is provided for topl_transactionFromMempool" in {
      httpPOST(
        modifierRequestBody("transactionId", "topl_transactionById", blocks.last.id.toString)
      ) ~> route ~> check {
        val res: String = parse(responseAs[String]).value.hcursor.downField("error").as[Json].toString
        res should include("The requested id's type is not an id type for Transaction")
      }
    }

    "Return correct error response when a transaction id is provided for topl_blockById" in {
      httpPOST(modifierRequestBody("blockId", "topl_blockById", txId)) ~> route ~> check {
        val res: String = parse(responseAs[String]).value.hcursor.downField("error").as[Json].toString
        res should include("The requested id's type is not an id type for Block")
      }
    }

    "Return correct error response when an id with non-base58 character is used for topl_transactionById" in {
      val invalidChar = "="
      val invalidCharId: String = invalidChar ++ txId.tail
      httpPOST(modifierRequestBody("transactionId", "topl_transactionById", invalidCharId)) ~> route ~> check {
        val res: String = parse(responseAs[String]).value.hcursor.downField("error").as[Json].toString
        res should include("failed to decode base-58 string").and(include(invalidChar))
      }
    }

    "Return correct error response when an id with non-base58 character is used for topl_transactionFromMempool" in {
      val invalidChar = "="
      val invalidCharId: String = invalidChar ++ txId.tail
      httpPOST(modifierRequestBody("transactionId", "topl_transactionFromMempool", invalidCharId)) ~> route ~> check {
        val res: String = parse(responseAs[String]).value.hcursor.downField("error").as[Json].toString
        res should include("failed to decode base-58 string").and(include(invalidChar))
      }
    }

    "Return correct error response when an id with non-base58 character is used for topl_blockById" in {
      val invalidChar = "="
      val invalidCharId: String = "=" ++ txId.tail
      httpPOST(modifierRequestBody("blockId", "topl_blockById", invalidCharId)) ~> route ~> check {
        val res: String = parse(responseAs[String]).value.hcursor.downField("error").as[Json].toString
        res should include("failed to decode base-58 string").and(include(invalidChar))
      }
    }

    "Return correct error response when an id with incorrect size is used for topl_transactionById" in {
      val invalidLengthId: String = txId.tail
      httpPOST(modifierRequestBody("transactionId", "topl_transactionById", invalidLengthId)) ~> route ~> check {
        val res: String = parse(responseAs[String]).value.hcursor.downField("error").as[Json].toString
        res should include("Modifier ID must be 33 bytes long")
      }
    }

    "Return correct error response when an id with incorrect size is used for topl_transactionFromMempool" in {
      val invalidLengthId: String = txId.tail
      httpPOST(modifierRequestBody("transactionId", "topl_transactionFromMempool", invalidLengthId)) ~> route ~> check {
        val res: String = parse(responseAs[String]).value.hcursor.downField("error").as[Json].toString
        res should include("Modifier ID must be 33 bytes long")
      }
    }

    "Return correct error response when an id with incorrect size is used for topl_blockById" in {
      val invalidLengthId: String = txId.tail
      httpPOST(modifierRequestBody("blockId", "topl_blockById", invalidLengthId)) ~> route ~> check {
        val res: String = parse(responseAs[String]).value.hcursor.downField("error").as[Json].toString
        res should include("Modifier ID must be 33 bytes long")
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
        |      "blockId": "${blocks.last.id.show}"
        |   }]
        |}
        |
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res: Json = parse(responseAs[String]).value
        res.hcursor.downField("result").as[Json].toString should include(blocks.last.id.toString)
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
        |      "blockIds": ${blocks.map(_.id).toList.asJson}
        |   }]
        |}
        |
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res: Json = parse(responseAs[String]).value
        blocks.map(_.id).map { blockId =>
          res.hcursor.downField("result").as[Json].toString should include(blockId.toString)
        }
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
        |      "blockIds": ["${blocks.last.id}", "${blockCurve25519Gen.sampleFirst().id}"]
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
        blockId shouldEqual blocks.head.id.toString
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
      val chainLength: Long = blocks.size
      val ranges: Seq[(Long, Long)] = Seq((0, 1), (2, 1), (chainLength + 1, chainLength + 1))
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
        blockIds.head shouldEqual blocks.head.id.toString
        res.hcursor.downField("error").values shouldBe None
      }
    }

    "Get a number of latest blocks" in {
      val requestBody = ByteString(s"""
        |{
        |   "jsonrpc": "2.0",
        |   "id": "1",
        |   "method": "topl_latestBlocks",
        |   "params": [{
        |      "numberOfBlocks": 1
        |    }]
        |}
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res: Json = parse(responseAs[String]).value
        val blocksRes = res.hcursor.downField("result").as[Json].value.toString
        blocksRes should include(blocks.last.id.toString)
        res.hcursor.downField("error").values shouldBe None
      }
    }

    "Get a number of latest block ids" in {
      val requestBody = ByteString(s"""
        |{
        |   "jsonrpc": "2.0",
        |   "id": "1",
        |   "method": "topl_latestBlockIds",
        |   "params": [{
        |      "numberOfBlockIds": 1
        |    }]
        |}
        """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res: Json = parse(responseAs[String]).value
        val blockIds = res.hcursor.downField("result").as[Seq[String]].value
        blockIds.head shouldEqual blocks.last.id.toString
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

        res.hcursor
          .get[String]("currentProtocolRuleset")
          .value
          .shouldEqual(protocolVersioner.applicable(view().history.height).toString)

        res.hcursor.downField("error").values shouldBe None
      }
    }

    "Return the confirmation status of transactions that are confirmed, pending, or not found" in {
      val unconfirmedTx = bifrostTransactionSeqGen.sampleFirst()
      val unconfirmedTxId = unconfirmedTx.head.id.toString
      val randomTx = bifrostTransactionSeqGen.sampleFirst()
      val randomTxId = randomTx.head.id.toString
      val requestBody = ByteString(s"""
        |{
        |   "jsonrpc": "2.0",
        |
        |   "id": "1",
        |   "method": "topl_confirmationStatus",
        |   "params": [{
        |      "transactionIds": ["$txId", "$unconfirmedTxId", "$randomTxId"]
        |   }]
        |}
        |
        """.stripMargin)

      view().mempool.putWithoutCheck(Seq(unconfirmedTx.head), blocks.last.timestamp)

      httpPOST(requestBody) ~> route ~> check {
        val res: Json = parse(responseAs[String]).value
        val confirmedStatus = res.hcursor.downField("result").get[Json](txId).value
        val unconfirmedStatus = res.hcursor.downField("result").get[Json](unconfirmedTxId).value
        val randomTxStatus = res.hcursor.downField("result").get[Json](randomTxId).value
        confirmedStatus.hcursor.downField("status").as[String].value shouldEqual "Confirmed"
        confirmedStatus.hcursor.downField("depthFromHead").as[Int].value shouldEqual 0
        unconfirmedStatus.hcursor.downField("status").as[String].value shouldEqual "Unconfirmed"
        unconfirmedStatus.hcursor.downField("depthFromHead").as[Int].value shouldEqual -1
        randomTxStatus.hcursor.downField("status").as[String].value shouldEqual "Not Found"
        randomTxStatus.hcursor.downField("depthFromHead").as[Int].value shouldEqual -1
        res.hcursor.downField("error").values shouldBe None
      }

      view().mempool.remove(unconfirmedTx.head)
    }
  }
}

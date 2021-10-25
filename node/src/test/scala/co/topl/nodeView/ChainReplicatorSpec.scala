package co.topl.nodeView

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import akka.actor.typed.ActorRef
import akka.actor.typed.eventstream.EventStream
import co.topl.attestation.Address
import co.topl.modifier.block.Block
import co.topl.modifier.box.ArbitBox
import co.topl.nodeView.ChainReplicatorSpec.TestInWithActor
import co.topl.nodeView.NodeViewHolder.ReceivableMessages
import co.topl.nodeView.NodeViewTestHelpers.TestIn
import co.topl.settings.ChainReplicatorSettings
import co.topl.tools.exporter.DataType
import co.topl.utils.{InMemoryKeyFileTestHelper, TestSettings, TimeProvider}
import com.mongodb.bulk.{BulkWriteResult, BulkWriteUpsert}
import io.circe._
import io.circe.parser._
import org.mongodb.scala.bson.BsonInt32
import org.scalamock.scalatest.MockFactory
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpecLike

import scala.collection.{mutable, AbstractIterator}
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

class ChainReplicatorSpec
    extends ScalaTestWithActorTestKit
    with AnyFlatSpecLike
    with TestSettings
    with InMemoryKeyFileTestHelper
    with NodeViewTestHelpers
    with MockFactory
    with OptionValues {

  behavior of "ChainReplicator"

  var blockStore: mutable.Map[Long, String] = scala.collection.mutable.Map[Long, String]()

  val chainRepSettings: ChainReplicatorSettings = settings.chainReplicator.copy(
    enableChainReplicator = true,
    checkMissingBlock = true,
    checkMissingStartHeight = 1,
    blockCheckSize = 10
  )

  it should "find and send the missing blocks to the database" in {

    implicit val timeProvider: TimeProvider = mock[TimeProvider]
    val blockNum = 15

    (() => timeProvider.time)
      .expects()
      .anyNumberOfTimes()
      .onCall(() => System.currentTimeMillis())

    genesisActorTest { testIn =>
      val nextBlocks = generateBlocks(List(genesisBlock), keyRingCurve25519.addresses.head).take(blockNum).toList
      testIn.nodeViewHolderRef.tell(ReceivableMessages.WriteBlocks(nextBlocks))

      Thread.sleep(0.5.seconds.toMillis)

      spawn(
        ChainReplicator(
          testIn.nodeViewHolderRef,
          () => checkValidationTest(),
          (start: Long, end: Long) => getExistingHeightsTest(start, end),
          (eleSeq: Seq[(String, String)], filterField: String, dt: DataType) =>
            replaceInsertTest(eleSeq, filterField, dt),
          chainRepSettings
        ),
        ChainReplicator.actorName
      )

      Thread.sleep(1.seconds.toMillis)
      blockStore.size shouldBe blockNum + 1
    }
  }

  it should "listen and send new blocks to the database" in {
    implicit val timeProvider: TimeProvider = mock[TimeProvider]
    val blockNum = 15

    (() => timeProvider.time)
      .expects()
      .anyNumberOfTimes()
      .onCall(() => System.currentTimeMillis())

    genesisActorTest { testIn =>
      val nextBlocks = generateBlocks(List(genesisBlock), keyRingCurve25519.addresses.head).take(blockNum).toList

      spawn(
        ChainReplicator(
          testIn.nodeViewHolderRef,
          () => checkValidationTest(),
          (start: Long, end: Long) => getExistingHeightsTest(start, end),
          (eleSeq: Seq[(String, String)], filterField: String, dt: DataType) =>
            replaceInsertTest(eleSeq, filterField, dt),
          chainRepSettings
        ),
        ChainReplicator.actorName
      )

      Thread.sleep(1.seconds.toMillis)

      nextBlocks.foreach { block =>
        system.eventStream.tell(EventStream.Publish(NodeViewHolder.Events.SemanticallySuccessfulModifier(block)))
      }

      Thread.sleep(1.seconds.toMillis)
      blockStore.size shouldBe blockNum + 1
    }
  }

  private def checkValidationTest(): Future[Seq[String]] = Future.successful(Seq("blocks", "transactions"))

  private def getExistingHeightsTest(start: Long, end: Long): Future[Seq[Long]] =
    Future.successful((start to end).filter(blockStore.contains(_)))

  private def replaceInsertTest(
    eleSeq:      Seq[(String, String)],
    filterField: String,
    dt:          DataType
  ): Future[BulkWriteResult] = {
    if (dt.name == "blocks")
      eleSeq.collect(ele =>
        parse(ele._2) match {
          case Right(json) =>
            val cursor: HCursor = json.hcursor
            val pair = for {
              blockId <- cursor.downField("id").as[String]
              height  <- cursor.downField("height").as[Long]
            } yield height -> blockId
            pair match {
              case Right(kv) => blockStore += kv
              case Left(err) => throw err
            }
        }
      )
    val bulkWriteUpsertList = (1 to eleSeq.size).map(ind => new BulkWriteUpsert(ind, BsonInt32(ind))).toList.asJava
    Future.successful(
      BulkWriteResult.acknowledged(0, 0, 0, 0, bulkWriteUpsertList, List().asJava)
    )
  }

  private def genesisActorTest(test: TestInWithActor => Unit)(implicit timeProvider: TimeProvider): Unit = {
    val testIn = genesisNodeView()
    val ref = spawn(NodeViewHolder(settings, () => Future.successful(testIn.nodeView)))
    val testInWithActor = TestInWithActor(testIn, ref)
    test(testInWithActor)
    testKit.stop(ref)
  }

  private def generateBlocks(previousBlocks: List[Block], forgerAddress: Address): Iterator[Block] =
    new AbstractIterator[Block] {

      // Because the reward fee is 0, the genesis arbit box is never destroyed during forging, so we can re-use it
      private val arbitBox =
        previousBlocks.last.transactions
          .flatMap(_.newBoxes)
          .collectFirst { case a: ArbitBox if a.evidence == forgerAddress.evidence => a }
          .value
      private var previous3Blocks: List[Block] = previousBlocks.takeRight(3)

      override def hasNext: Boolean = true

      override def next(): Block =
        if (previous3Blocks.isEmpty) {
          previous3Blocks = List(genesisBlock)
          genesisBlock
        } else {
          val newBlock = nextBlock(
            previous3Blocks.last,
            arbitBox,
            previous3Blocks.map(_.timestamp),
            forgerAddress
          )
          previous3Blocks = (previous3Blocks :+ newBlock).takeRight(3)
          newBlock
        }
    }
}

object ChainReplicatorSpec {
  case class TestInWithActor(testIn: TestIn, nodeViewHolderRef: ActorRef[NodeViewHolder.ReceivableMessage])
}

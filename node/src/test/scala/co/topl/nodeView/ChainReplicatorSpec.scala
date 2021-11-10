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
import co.topl.utils.{InMemoryKeyFileTestHelper, TestSettings, TimeProvider}
import com.mongodb.client.result.{DeleteResult, InsertManyResult}
import org.bson.BsonValue
import org.mongodb.scala.bson.Document
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

  val blockCollectionName: String = settings.chainReplicator.blockCollection
  val confirmedTxCollectionName: String = settings.chainReplicator.confirmedTxCollection
  val unconfirmedTxCollectionName: String = settings.chainReplicator.unconfirmedTxCollection
  var blockStore: mutable.Map[String, String] = scala.collection.mutable.Map[String, String]()
  var confirmedTxStore: mutable.Map[String, String] = scala.collection.mutable.Map[String, String]()
  var unconfirmedTxStore: mutable.Map[String, String] = scala.collection.mutable.Map[String, String]()

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

      val chainRepRef = spawn(
        ChainReplicator(
          testIn.nodeViewHolderRef,
          () => checkValidationTest(),
          (eleSeq: Seq[Document], collectionName: String) => insertDBTest(eleSeq, collectionName),
          (field: String, value: Seq[String], collectionName: String) => removeDBTest(field, value, collectionName),
          (collectionName: String) => getUnconfirmedTxTest(collectionName),
          (idsToCheck: Seq[String], collectionName: String) => getMissingBlockIdsTest(idsToCheck, collectionName),
          chainRepSettings
        ),
        ChainReplicator.actorName
      )

      Thread.sleep(1.seconds.toMillis)
      blockStore.size shouldBe blockNum + 1
      chainRepRef ! ChainReplicator.ReceivableMessages.Terminate(new Exception("stopping first chain replicator"))
    }
  }

  it should "listen and send new blocks to the database" in {
    implicit val timeProvider: TimeProvider = mock[TimeProvider]
    val blockNum = 15
    blockStore = scala.collection.mutable.Map[String, String]()

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
          (eleSeq: Seq[Document], collectionName: String) => insertDBTest(eleSeq, collectionName),
          (field: String, value: Seq[String], collectionName: String) => removeDBTest(field, value, collectionName),
          (collectionName: String) => getUnconfirmedTxTest(collectionName),
          (idsToCheck: Seq[String], collectionName: String) => getMissingBlockIdsTest(idsToCheck, collectionName),
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

  private def insertDBTest(
    eleSeq:         Seq[Document],
    collectionName: String
  ): Future[InsertManyResult] = {
    collectionName match {
      case `blockCollectionName` =>
        eleSeq.foreach { ele =>
          val id = ele.get("id").head.asString().getValue
          val height = ele.get("height").head.asNumber().longValue().toString
          blockStore += (id -> height)
        }

      case `confirmedTxCollectionName` =>
        eleSeq.foreach { ele =>
          val id = ele.get("txId").head.asString().getValue
          val block = ele.get("timestamp").head.asString().getValue
          confirmedTxStore += (id -> block)
        }
      case `unconfirmedTxCollectionName` =>
        eleSeq.foreach { ele =>
          val id = ele.get("txId").head.asString().getValue
          val timestamp = ele.get("timestamp").head.asString().getValue
          unconfirmedTxStore += (id -> timestamp)
        }
    }

    val insertedIds = Map[Integer, BsonValue]().asJava
    Future.successful(InsertManyResult.acknowledged(insertedIds))
  }

  private def removeDBTest(field: String, value: Seq[String], collectionName: String): Future[DeleteResult] = {
    var count = 0
    value.foreach{ key =>
      if (collectionName == settings.chainReplicator.unconfirmedTxCollection) {
        unconfirmedTxStore.remove(key)
        count += 1
      }
    }
    Future.successful(DeleteResult.acknowledged(count))
  }

  private def getUnconfirmedTxTest(collectionName: String): Future[Seq[String]] =
    Future.successful(unconfirmedTxStore.keys.toSeq)


  private def getMissingBlockIdsTest(idsToCheck: Seq[String], collectionName: String): Future[Seq[String]] = {
    Future.successful(idsToCheck.filterNot(blockStore.contains))
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

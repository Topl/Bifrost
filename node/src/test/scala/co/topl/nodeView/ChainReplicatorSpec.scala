package co.topl.nodeView

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import akka.actor.typed.ActorRef
import akka.actor.typed.eventstream.EventStream
import co.topl.attestation.{Address, PublicKeyPropositionCurve25519}
import co.topl.consensus.{ActorConsensusInterface, NxtConsensus}
import co.topl.modifier.block.Block
import co.topl.modifier.box.ArbitBox
import co.topl.modifier.transaction.builder.{BoxSelectionAlgorithms, TransferBuilder, TransferRequests}
import co.topl.nodeView.ChainReplicatorSpec.TestInWithActor
import co.topl.nodeView.NodeViewHolder.ReceivableMessages
import co.topl.nodeView.NodeViewTestHelpers.TestIn
import co.topl.nodeView.history.InMemoryKeyValueStore
import co.topl.nodeView.state.{MinimalState, State}
import co.topl.settings.ChainReplicatorSettings
import co.topl.tools.exporter.DatabaseOperations
import co.topl.utils.IdiomaticScalaTransition.implicits.toEitherOps
import co.topl.utils.mongodb.DocumentEncoder
import co.topl.utils.mongodb.models.{BlockDataModel, ConfirmedTransactionDataModel, UnconfirmedTransactionDataModel}
import co.topl.utils.{InMemoryKeyFileTestHelper, TestSettings, TimeProvider}
import com.mongodb.client.result.{DeleteResult, InsertManyResult}
import org.bson.BsonValue
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

    blockStore = scala.collection.mutable.Map[String, String]()
    confirmedTxStore = scala.collection.mutable.Map[String, String]()
    unconfirmedTxStore = scala.collection.mutable.Map[String, String]()

    (() => timeProvider.time)
      .expects()
      .anyNumberOfTimes()
      .onCall(() => System.currentTimeMillis())

    genesisActorTest { testIn =>
      val newBlocks = generateBlocks(List(genesisBlock), keyRingCurve25519.addresses.head).take(blockNum).toList
      testIn.nodeViewHolderRef.tell(ReceivableMessages.WriteBlocks(newBlocks))
      val newTxs = newBlocks.flatMap(_.transactions)

      Thread.sleep(0.5.seconds.toMillis)

      val dbOps = mock[DatabaseOperations]

      (() => dbOps.checkValidConnection())
        .expects()
        .once()
        .onCall(_ => checkValidationTest)

      (dbOps
        .insert[BlockDataModel](_: Seq[BlockDataModel], _: String)(_: DocumentEncoder[BlockDataModel]))
        .expects(*, chainRepSettings.blockCollection, *)
        .anyNumberOfTimes()
        .onCall((docSeq, _, _) => insertBlockDBTest(docSeq))

      (dbOps
        .insert[ConfirmedTransactionDataModel](_: Seq[ConfirmedTransactionDataModel], _: String)(
          _: DocumentEncoder[ConfirmedTransactionDataModel]
        ))
        .expects(*, chainRepSettings.confirmedTxCollection, *)
        .anyNumberOfTimes()
        .onCall((docSeq, _, _) => insertConfirmedTXDBTest(docSeq))

      (dbOps
        .insert[UnconfirmedTransactionDataModel](_: Seq[UnconfirmedTransactionDataModel], _: String)(
          _: DocumentEncoder[UnconfirmedTransactionDataModel]
        ))
        .expects(*, chainRepSettings.unconfirmedTxCollection, *)
        .anyNumberOfTimes()
        .onCall((docSeq, _, _) => insertUnconfirmedTXDBTest(docSeq))

      (dbOps.getUnconfirmedTxs _)
        .expects(*)
        .anyNumberOfTimes()
        .onCall((_: String) => getUnconfirmedTxTest)

      (dbOps.getExistingIds _)
        .expects(*, *)
        .anyNumberOfTimes()
        .onCall((idsToCheck, _) => getExistingIdsTest(idsToCheck))

      val chainRepRef = spawn(
        ChainReplicator(
          testIn.nodeViewHolderRef,
          dbOps,
          chainRepSettings
        ),
        ChainReplicator.actorName
      )

      Thread.sleep(0.5.seconds.toMillis)
      blockStore.size shouldBe newBlocks.size + 1
      confirmedTxStore.size shouldBe newTxs.size + genesisBlock.transactions.size
      testKit.stop(chainRepRef)
    }
  }

  it should "listen and send new blocks to the database" in {
    implicit val timeProvider: TimeProvider = mock[TimeProvider]
    val blockNum = 15

    blockStore = scala.collection.mutable.Map[String, String]()
    confirmedTxStore = scala.collection.mutable.Map[String, String]()
    unconfirmedTxStore = scala.collection.mutable.Map[String, String]()

    (() => timeProvider.time)
      .expects()
      .anyNumberOfTimes()
      .onCall(() => System.currentTimeMillis())

    genesisActorTest { testIn =>
      val newBlocks = generateBlocks(List(genesisBlock), keyRingCurve25519.addresses.head).take(blockNum).toList
      val newTxs = newBlocks.flatMap(_.transactions)

      val dbOps = mock[DatabaseOperations]

      (() => dbOps.checkValidConnection())
        .expects()
        .once()
        .onCall(_ => checkValidationTest)

      (dbOps
        .insert[BlockDataModel](_: Seq[BlockDataModel], _: String)(_: DocumentEncoder[BlockDataModel]))
        .expects(*, chainRepSettings.blockCollection, *)
        .anyNumberOfTimes()
        .onCall((docSeq, _, _) => insertBlockDBTest(docSeq))

      (dbOps
        .insert[ConfirmedTransactionDataModel](_: Seq[ConfirmedTransactionDataModel], _: String)(
          _: DocumentEncoder[ConfirmedTransactionDataModel]
        ))
        .expects(*, chainRepSettings.confirmedTxCollection, *)
        .anyNumberOfTimes()
        .onCall((docSeq, _, _) => insertConfirmedTXDBTest(docSeq))

      (dbOps
        .insert[UnconfirmedTransactionDataModel](_: Seq[UnconfirmedTransactionDataModel], _: String)(
          _: DocumentEncoder[UnconfirmedTransactionDataModel]
        ))
        .expects(*, chainRepSettings.unconfirmedTxCollection, *)
        .anyNumberOfTimes()
        .onCall((docSeq, _, _) => insertUnconfirmedTXDBTest(docSeq))

      (dbOps.getUnconfirmedTxs _)
        .expects(*)
        .anyNumberOfTimes()
        .onCall((_: String) => getUnconfirmedTxTest)

      (dbOps.getExistingIds _)
        .expects(*, *)
        .anyNumberOfTimes()
        .onCall((idsToCheck, _) => getExistingIdsTest(idsToCheck))

      (dbOps.remove _)
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall((_, values, collectionName) => removeUnconfirmedTxDBTest(values, collectionName))

      val chainRepRef = spawn(
        ChainReplicator(
          testIn.nodeViewHolderRef,
          dbOps,
          chainRepSettings
        ),
        ChainReplicator.actorName
      )

      Thread.sleep(0.5.seconds.toMillis)

      newBlocks.foreach { block =>
        system.eventStream.tell(EventStream.Publish(NodeViewHolder.Events.SemanticallySuccessfulModifier(block)))
      }

      Thread.sleep(0.5.seconds.toMillis)
      blockStore.size shouldBe newBlocks.size + 1
      confirmedTxStore.size shouldBe newTxs.size + genesisBlock.transactions.size
      testKit.stop(chainRepRef)
    }
  }

  it should "Keep the unconfirmed transactions in appView the same as the ones in mempool" in {
    implicit val timeProvider: TimeProvider = mock[TimeProvider]

    blockStore = scala.collection.mutable.Map[String, String]()
    confirmedTxStore = scala.collection.mutable.Map[String, String]()
    unconfirmedTxStore = scala.collection.mutable.Map[String, String]()

    (() => timeProvider.time)
      .expects()
      .anyNumberOfTimes()
      .onCall(() => System.currentTimeMillis())

    genesisActorTest { testIn =>
      val addressA :: addressB :: _ = keyRingCurve25519.addresses.toList
      val rand = scala.util.Random
      def polyTransferParams(
        transferAmount: Int
      ): (MinimalState[Block, State], TransferRequests.PolyTransferRequest, BoxSelectionAlgorithms.All.type) = (
        testIn.testIn.nodeView.state,
        TransferRequests.PolyTransferRequest(
          List(addressB),
          List(addressA -> transferAmount),
          addressB,
          0,
          None
        ),
        BoxSelectionAlgorithms.All
      )
      val polyTransferFst = {
        val base = (TransferBuilder.buildUnsignedPolyTransfer[PublicKeyPropositionCurve25519] _)
          .tupled(polyTransferParams(rand.nextInt(20)))
          .getOrThrow()
        base.copy(attestation = keyRingCurve25519.generateAttestation(addressB)(base.messageToSign))
      }
      val polyTransferSec = {
        val base = (TransferBuilder.buildUnsignedPolyTransfer[PublicKeyPropositionCurve25519] _)
          .tupled(polyTransferParams(rand.nextInt(20)))
          .getOrThrow()
        base.copy(attestation = keyRingCurve25519.generateAttestation(addressB)(base.messageToSign))
      }
      val polyTransferTrd = {
        val base = (TransferBuilder.buildUnsignedPolyTransfer[PublicKeyPropositionCurve25519] _)
          .tupled(polyTransferParams(rand.nextInt(20)))
          .getOrThrow()
        base.copy(attestation = keyRingCurve25519.generateAttestation(addressB)(base.messageToSign))
      }

      val dbOps = mock[DatabaseOperations]

      (() => dbOps.checkValidConnection())
        .expects()
        .once()
        .onCall(_ => checkValidationTest)

      (dbOps
        .insert[BlockDataModel](_: Seq[BlockDataModel], _: String)(_: DocumentEncoder[BlockDataModel]))
        .expects(*, chainRepSettings.blockCollection, *)
        .anyNumberOfTimes()
        .onCall((docSeq, _, _) => insertBlockDBTest(docSeq))

      (dbOps
        .insert[ConfirmedTransactionDataModel](_: Seq[ConfirmedTransactionDataModel], _: String)(
          _: DocumentEncoder[ConfirmedTransactionDataModel]
        ))
        .expects(*, chainRepSettings.confirmedTxCollection, *)
        .anyNumberOfTimes()
        .onCall((docSeq, _, _) => insertConfirmedTXDBTest(docSeq))

      (dbOps
        .insert[UnconfirmedTransactionDataModel](_: Seq[UnconfirmedTransactionDataModel], _: String)(
          _: DocumentEncoder[UnconfirmedTransactionDataModel]
        ))
        .expects(*, chainRepSettings.unconfirmedTxCollection, *)
        .anyNumberOfTimes()
        .onCall((docSeq, _, _) => insertUnconfirmedTXDBTest(docSeq))

      (dbOps.getUnconfirmedTxs _)
        .expects(*)
        .anyNumberOfTimes()
        .onCall((_: String) => getUnconfirmedTxTest)

      (dbOps.getExistingIds _)
        .expects(*, *)
        .anyNumberOfTimes()
        .onCall((idsToCheck, _) => getExistingIdsTest(idsToCheck))

      (dbOps.remove _)
        .expects(*, *, *)
        .anyNumberOfTimes()
        .onCall((_, values, collectionName) => removeUnconfirmedTxDBTest(values, collectionName))

      val chainRepRef = spawn(
        ChainReplicator(
          testIn.nodeViewHolderRef,
          dbOps,
          chainRepSettings
        ),
        ChainReplicator.actorName
      )
      testIn.nodeViewHolderRef.tell(
        NodeViewHolder.ReceivableMessages.WriteTransactions(List(polyTransferFst, polyTransferSec))
      )
      Thread.sleep(0.5.seconds.toMillis)
      system.eventStream.tell(EventStream.Publish(NodeViewHolder.Events.SemanticallySuccessfulModifier(genesisBlock)))
      Thread.sleep(0.5.seconds.toMillis)
      unconfirmedTxStore.keys.toSet == Set(polyTransferFst.id.toString, polyTransferSec.id.toString) shouldBe true
      testIn.nodeViewHolderRef ! NodeViewHolder.ReceivableMessages.EliminateTransactions(Seq(polyTransferFst.id))
      testIn.nodeViewHolderRef.tell(NodeViewHolder.ReceivableMessages.WriteTransactions(List(polyTransferTrd)))
      system.eventStream.tell(EventStream.Publish(NodeViewHolder.Events.SemanticallySuccessfulModifier(genesisBlock)))
      Thread.sleep(0.5.seconds.toMillis)
      unconfirmedTxStore.keys.toSet == Set(polyTransferSec.id.toString, polyTransferTrd.id.toString) shouldBe true
      testKit.stop(chainRepRef)
    }
  }

  private def checkValidationTest: Future[Seq[String]] = Future.successful(Seq("blocks", "transactions"))

  private def insertBlockDBTest(eleSeq: Seq[BlockDataModel]): Future[InsertManyResult] = {
    eleSeq.foreach { ele =>
      val id = ele.id
      val height = ele.height.toString
      blockStore += (id -> height)
    }
    val insertedIds = Map[Integer, BsonValue]().asJava
    Future.successful(InsertManyResult.acknowledged(insertedIds))
  }

  private def insertConfirmedTXDBTest(eleSeq: Seq[ConfirmedTransactionDataModel]): Future[InsertManyResult] = {
    eleSeq.foreach { ele =>
      val id = ele.txId
      val timestamp = ele.timestamp
      confirmedTxStore += (id -> timestamp)
    }
    val insertedIds = Map[Integer, BsonValue]().asJava
    Future.successful(InsertManyResult.acknowledged(insertedIds))
  }

  private def insertUnconfirmedTXDBTest(eleSeq: Seq[UnconfirmedTransactionDataModel]): Future[InsertManyResult] = {
    eleSeq.foreach { ele =>
      val id = ele.txId
      val timestamp = ele.timestamp
      unconfirmedTxStore += (id -> timestamp)
    }
    val insertedIds = Map[Integer, BsonValue]().asJava
    Future.successful(InsertManyResult.acknowledged(insertedIds))
  }

  private def removeUnconfirmedTxDBTest(value: Seq[String], collectionName: String): Future[DeleteResult] = {
    var count = 0
    value.foreach { key =>
      if (collectionName == settings.chainReplicator.unconfirmedTxCollection) {
        unconfirmedTxStore.remove(key)
        count += 1
      }
    }
    Future.successful(DeleteResult.acknowledged(count))
  }

  private def getUnconfirmedTxTest: Future[Seq[String]] =
    Future.successful(unconfirmedTxStore.keys.toSeq)

  private def getExistingIdsTest(idsToCheck: Seq[String]): Future[Seq[String]] =
    Future.successful(idsToCheck.filter(blockStore.contains))

  private def genesisActorTest(test: TestInWithActor => Unit)(implicit timeProvider: TimeProvider): Unit = {
    val testIn = genesisNodeView()
    val consensusStorageRef = spawn(
      NxtConsensus(
        settings,
        appContext.networkType,
        InMemoryKeyValueStore.empty()
      ),
      NxtConsensus.actorName
    )
    val nodeViewHolderRef = spawn(
      NodeViewHolder(
        settings,
        new ActorConsensusInterface(consensusStorageRef),
        () => Future.successful(testIn.nodeView)
      )
    )
    val testInWithActor = TestInWithActor(testIn, nodeViewHolderRef, consensusStorageRef)
    test(testInWithActor)
    testKit.stop(nodeViewHolderRef)
    testKit.stop(consensusStorageRef)
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

  case class TestInWithActor(
    testIn:              TestIn,
    nodeViewHolderRef:   ActorRef[NodeViewHolder.ReceivableMessage],
    consensusStorageRef: ActorRef[NxtConsensus.ReceivableMessage]
  )
}

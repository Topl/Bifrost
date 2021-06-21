package co.topl.storage.blockchain

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import akka.stream.scaladsl.Sink
import cats.data._
import cats.implicits._
import cats.scalatest.FutureEitherValues
import co.topl.storage.graph.OrientDBGraph
import co.topl.storage.leveldb.LevelDBStore
import co.topl.storage.mapdb.MapDBStore
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, OptionValues}

import java.nio.file.Files
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class BlockchainGraphSpec
    extends ScalaTestWithActorTestKit
    with AnyFlatSpecLike
    with BeforeAndAfterAll
    with FutureEitherValues
    with OptionValues
    with Matchers {

  behavior of "BlockchainGraphSpec"

  implicit val ec: ExecutionContext = system.executionContext

  implicit override val patienceConfig: PatienceConfig = PatienceConfig(2.hours)

  private var graph: OrientDBGraph = _
  private var underTest: BlockchainGraph = _

  private val blockId1 = "block1"
  private val transactionId1 = "tx1"
  private val boxId1 = "box1"

  private val blockId2 = "block2"
  private val transactionId2 = "tx2"
  private val boxId2 = "box2"

  private val blockHeader1 = BlockHeader(
    blockId = blockId1,
    timestamp = 1,
    publicKey = "topl",
    signature = "topl",
    height = 1,
    difficulty = 1,
    txRoot = "bar",
    bloomFilter = "baz",
    version = 1
  )

  private val blockBody1 = BlockBody(blockId1)

  private val transaction1 = Transaction(
    transactionId = transactionId1,
    fee = "100",
    timestamp = 1,
    data = None,
    minting = true,
    attestation = Map("topl" -> "BobSaidSo")
  )

  private val box1 = Box(
    boxId = boxId1,
    boxType = 1,
    value = "7",
    nonce = 1
  )

  private val blockHeader2 = BlockHeader(
    blockId = blockId2,
    timestamp = 2,
    publicKey = "topl",
    signature = "topl",
    height = 2,
    difficulty = 1,
    txRoot = "bar",
    bloomFilter = "baz",
    version = 1
  )

  private val blockBody2 = BlockBody(blockId2)

  private val transaction2 = Transaction(
    transactionId = transactionId2,
    fee = "100",
    timestamp = 2,
    data = None,
    minting = false,
    attestation = Map("topl" -> "BobSaidSo")
  )

  private val box2 = Box(
    boxId = boxId2,
    boxType = 1,
    value = "7",
    nonce = 1
  )

  it should "Add BlockHeader 1" in {
    val t = underTest
    import t._

    NonEmptyChain(CreateBlockHeader(blockHeader1)).run().futureRightValue

    blockId1.blockHeader.futureRightValue shouldBe blockHeader1
  }

  it should "Add a Head" in {
    val t = underTest
    import t._

    NonEmptyChain(SetHead(blockId1)).run().futureRightValue

    Blockchain.currentHead.futureRightValue shouldBe blockHeader1
  }

  it should "have a genesis block" in {
    val t = underTest
    import t._

    Blockchain.genesis.futureRightValue shouldBe blockHeader1
  }

  it should "know blocks at height=1" in {
    val t = underTest
    import t._

    val blocks =
      Blockchain.blocksAtHeight(1).runWith(Sink.seq).futureValue.toList.map(_.value)

    blocks should (have size 1 and contain(blockHeader1))
  }

  it should "Add BlockBody 1" in {
    val t = underTest
    import t._

    NonEmptyChain(CreateBlockBody(blockBody1), AssociateBodyToHeader(blockId1, blockId1))
      .run()
      .futureRightValue

    blockId1.blockBody.futureRightValue shouldBe blockBody1
    blockBody1.header.futureRightValue shouldBe blockHeader1
    blockHeader1.body.futureRightValue shouldBe blockBody1
  }

  it should "Add Transaction 1" in {
    val t = underTest
    import t._

    NonEmptyChain(CreateTransaction(transaction1), AssociateTransactionToBody(transactionId1, blockId1, index = 0))
      .run()
      .futureRightValue

    transactionId1.transaction.futureRightValue shouldBe transaction1

    val transactions =
      blockBody1.transactions.runWith(Sink.seq).futureValue.toList.map(_.value)

    transactions should (have size 1 and contain(transaction1))
  }

  it should "Add Box 1" in {
    val t = underTest
    import t._

    NonEmptyChain(CreateBox(box1), AssociateBoxCreator(boxId1, transactionId1, minted = true))
      .run()
      .futureRightValue

    val transactionCreates =
      transaction1.creates.runWith(Sink.seq).futureValue.toList.map(_.value)

    transactionCreates should (have size 1 and contain(box1))

    val boxCreatedBy =
      box1.createdBy.futureRightValue

    boxCreatedBy shouldBe transaction1
  }

  it should "Not have block2 yet" in {
    val t = underTest
    import t._

    blockId2.blockHeader.futureLeftValue shouldBe BlockchainData.NotFound
  }

  it should "Add Block, Transaction, and Box 2" in {
    val t = underTest
    import t._
    NonEmptyChain(
      CreateBlockHeader(blockHeader2),
      AssociateBlockToParent(blockId2, blockId1),
      CreateBlockBody(blockBody2),
      AssociateBodyToHeader(blockId2, blockId2),
      CreateTransaction(transaction2),
      AssociateTransactionToBody(transactionId2, blockId2, index = 0),
      AssociateBoxOpener(boxId1, transactionId2),
      CreateBox(box2),
      AssociateBoxCreator(boxId2, transactionId2, minted = false),
      SetHead(blockId2)
    ).run().futureRightValue

    blockHeader2.parentBlock.futureRightValue shouldBe blockHeader1
    blockHeader1.childBlocks.runWith(Sink.seq).futureValue.toList.map(_.value) should (have size 1 and contain(
      blockHeader2
    ))
    blockHeader2.body.futureRightValue shouldBe blockBody2
    blockBody2.header.futureRightValue shouldBe blockHeader2
    blockBody2.transactions.runWith(Sink.seq).futureValue.toList.map(_.value) should (have size 1 and contain(
      transaction2
    ))
    transaction2.blockBody.futureRightValue shouldBe blockBody2
    transaction2.opens.runWith(Sink.seq).futureValue.toList.map(_.value) should (have size 1 and contain(
      box1
    ))
    transaction2.creates.runWith(Sink.seq).futureValue.toList.map(_.value) should (have size 1 and contain(
      box2
    ))
    box2.createdBy.futureRightValue shouldBe transaction2
    box1.openedBy.runWith(Sink.seq).futureValue.toList.map(_.value) should (have size 1 and contain(
      transaction2
    ))

    val originalBoxBlock =
      (for {
        body               <- blockHeader2.body
        transaction        <- EitherT(body.transactions.runWith(Sink.head))
        opensBox           <- EitherT(transaction.opens.runWith(Sink.head))
        createdBy          <- opensBox.createdBy
        creatorBlockBody   <- createdBy.blockBody
        creatorBlockHeader <- creatorBlockBody.header
      } yield creatorBlockHeader).futureRightValue

    originalBoxBlock shouldBe blockHeader1
  }

  it should "retrieve history" in {
    val t = underTest
    import t._

    blockHeader2.history.runWith(Sink.seq).futureValue.toList.map(_.value) should (have size 1 and contain(
      blockHeader1
    ))
  }

  it should "find a specific unopened box for a given block/state" in {
    val t = underTest
    import t._

    blockBody1.lookupUnopenedBox(boxId1).futureRightValue shouldBe box1
    blockBody1.lookupUnopenedBox(boxId2).futureLeftValue shouldBe BlockchainData.NotFound
    blockBody2.lookupUnopenedBox(boxId1).futureLeftValue shouldBe BlockchainData.NotFound
    blockBody2.lookupUnopenedBox(boxId2).futureRightValue shouldBe box2
  }

  it should "save a snapshot at a specific block" in {
    val t = underTest
    import t._
    NonEmptyChain(CreateState(blockId2)).run().futureRightValue

    blockBody1.state.futureLeftValue shouldBe BlockchainData.NotFound

    val unopenedBoxes =
      blockBody2.state.futureRightValue.unopenedBoxIds
        .runWith(Sink.seq)
        .futureValue
        .map(_.value)

    unopenedBoxes should (have size 1 and contain(boxId2))
    unopenedBoxes should not contain box1
  }

  it should "know the current heads" in {
    val t = underTest
    import t._

    Blockchain.currentHead.futureRightValue shouldBe blockHeader2
    Blockchain.currentHeads.runWith(Sink.seq).futureValue.toList.map(_.value) should (have size 1 and contain(
      blockHeader2
    ))

    Blockchain.genesis.futureRightValue shouldBe blockHeader1
  }

  override def beforeAll(): Unit = {
    super.beforeAll()

    val schema = BlockchainGraphSchema.value

    graph = OrientDBGraph(schema, OrientDBGraph.InMemory)

    underTest = new BlockchainGraph()(system, graph, new LevelDBStore(Files.createTempDirectory("BlockchainGraphSpec")))
  }

  override def afterAll(): Unit = {
    underTest.close()
    super.afterAll()
  }
}

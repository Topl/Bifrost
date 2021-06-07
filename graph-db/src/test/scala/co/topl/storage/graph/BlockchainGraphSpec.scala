package co.topl.storage.graph

import akka.actor.ActorSystem
import akka.stream.scaladsl.Sink
import akka.testkit.TestKit
import cats.data.{Chain, EitherT, NonEmptyChain}
import cats.implicits._
import cats.scalatest.FutureEitherValues
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, OptionValues}

import scala.concurrent.duration._

class BlockchainGraphSpec
    extends TestKit(ActorSystem("OrientDbBlockchainGraphSpec"))
    with AnyFlatSpecLike
    with BeforeAndAfterAll
    with FutureEitherValues
    with OptionValues
    with Matchers {

  import system.dispatcher

  behavior of "OrientDbBlockchainGraph"

  implicit override val patienceConfig: PatienceConfig = PatienceConfig(2.minutes)

  private var underTest: BlockchainOpsProvider = _

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
    minting = true
  )

  private val box1 = Box(
    boxId = boxId1,
    boxType = "PolyBox",
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
    minting = false
  )

  private val box2 = Box(
    boxId = boxId2,
    boxType = "PolyBox",
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

  it should "Add BlockBody 1" in {
    val t = underTest
    import t._

    NonEmptyChain(CreateBlockBody(blockBody1), AssociateBodyToHeader(blockId1, blockId1)).run().value.futureValue.value

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

    blockId2.blockHeader.futureLeftValue shouldBe BlockchainOps.NotFound
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
      AssociateBoxOpener(boxId1, transactionId2, attestation = "BobSaidSo"),
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

  it should "retrieve state" in {
    val t = underTest
    import t._

    blockBody1.lookupUnopenedBox(boxId1).futureRightValue shouldBe box1
    blockBody1.lookupUnopenedBox(boxId2).futureLeftValue shouldBe BlockchainOps.NotFound
//    blockBody1.state.futureRightValue.unopenedBoxes
//      .runWith(Sink.seq)
//      .futureValue
//      .map(_.value) should (have size 1 and contain(box1))
    blockBody2.lookupUnopenedBox(boxId1).futureLeftValue shouldBe BlockchainOps.NotFound
    blockBody2.lookupUnopenedBox(boxId2).futureRightValue shouldBe box2
//    blockBody2.state.futureRightValue.unopenedBoxes
//      .runWith(Sink.seq)
//      .futureValue
//      .map(_.value) should (have size 1 and contain(box2))
  }

  it should "know the current heads" in {
    val t = underTest
    import t._

    // TODO: OrientDB hasn't caught up yet for some reason, so without a slight delay, the following queries may fail
    Thread.sleep(2000)

    Blockchain.currentHead.futureRightValue shouldBe blockHeader2
    Blockchain.currentHeads.runWith(Sink.seq).futureValue.toList.map(_.value) should (have size 1 and contain(
      blockHeader2
    ))

    Blockchain.genesis.futureRightValue shouldBe blockHeader1
  }

  it should "retrieve a long history" in {
    val t = underTest
    import t._

    val modifications =
      (3 to 500).flatMap { i =>
        val id = s"block$i"
        val header = BlockHeader(
          blockId = id,
          timestamp = i,
          publicKey = "topl",
          signature = "topl",
          height = i,
          difficulty = 1,
          txRoot = "bar",
          bloomFilter = "baz",
          version = 1
        )

        List(CreateBlockHeader(header), AssociateBlockToParent(id, s"block${i - 1}"))
      } :+ SetHead("block500")

    NonEmptyChain.fromChainUnsafe(Chain.fromSeq(modifications)).run().futureRightValue

    val head = Blockchain.currentHead.futureRightValue

    head.blockId shouldBe "block500"

    val history =
      head.history.runWith(Sink.seq).futureValue.toList.map(_.value)

    history should have size 499
    history.zip(500 to 1).foreach { case (header, index) =>
      header.blockId shouldBe s"block${index + 1}"
    }
  }

  override def beforeAll(): Unit = {
    super.beforeAll()

    val schema = BlockchainGraphSchema.value

    underTest = new BlockchainGraph(OrientDBGraph(schema, OrientDBGraph.InMemory))
  }

  override def afterAll(): Unit = {
    super.afterAll()
    system.terminate().futureValue(Timeout(20.seconds))
  }
}

package co.topl.storage.graph

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import akka.stream.scaladsl.{Sink, Source}
import cats.data.NonEmptyChain
import cats.scalatest.FutureEitherValues
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, DoNotDiscover, OptionValues}
import org.slf4j.{Logger, LoggerFactory}

import java.nio.file.{Files, Path, Paths}
import java.util.Comparator
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

@DoNotDiscover
class BlockchainGraphStatePerfSpec
    extends ScalaTestWithActorTestKit
    with AnyFlatSpecLike
    with BeforeAndAfterAll
    with FutureEitherValues
    with OptionValues
    with Matchers {

  behavior of "BlockchainGraphHistoryPerfSpec"

  private val logger: Logger = LoggerFactory.getLogger(this.getClass)

  implicit val ec: ExecutionContext = system.executionContext

  implicit override val patienceConfig: PatienceConfig = PatienceConfig(Duration.Inf)

  private var dataDir: Path = _
  private var graph: OrientDBGraph = _
  private var underTest: BlockchainData = _

  private val count = 5000

  it should "contain a block" in {
    val t = underTest
    import t._

    val head = Blockchain.currentHead.futureRightValue

    head.blockId shouldBe s"${count + 1}_1"

    val body = head.body.futureRightValue

    body.blockId shouldBe head.blockId

    body.lookupUnopenedBox("1_1_1").futureLeftValue shouldBe BlockchainData.NotFound
    body.lookupUnopenedBox("2_1_2").futureRightValue shouldBe Box("2_1_2", 1, "1", 1)
  }

  it should "find opened and unopened boxes" in {
    val t = underTest
    import t._
    val headBody = Blockchain.currentHead.flatMap(_.body).futureRightValue

    headBody.lookupUnopenedBox("1_1_1").futureLeftValue shouldBe BlockchainData.NotFound
    headBody.lookupUnopenedBox("2_1_2").futureRightValue shouldBe Box("2_1_2", 1, "1", 1)
  }

  it should "optimize a state lookup with a snapshot" in {
    val t = underTest
    import t._

    val headBody = Blockchain.currentHead.flatMap(_.body).futureRightValue

    val targetBlockId = s"${count}_1"

    NonEmptyChain(CreateState(targetBlockId))

    headBody.lookupUnopenedBox("1_1_1").futureLeftValue shouldBe BlockchainData.NotFound
    headBody.lookupUnopenedBox("2_1_2").futureRightValue shouldBe Box("2_1_2", 1, "1", 1)
  }

  override def beforeAll(): Unit = {
    super.beforeAll()

    val schema = BlockchainGraphSchema.value

    dataDir = Paths.get(".", "target", "test", "db" + System.currentTimeMillis().toString)

    graph = OrientDBGraph(schema, OrientDBGraph.Local(dataDir))

    underTest = new BlockchainGraph(graph)

    prepareGraph()
  }

  override def afterAll(): Unit = {
    super.afterAll()
    graph.close()

    Files
      .walk(dataDir)
      .sorted(Comparator.reverseOrder[Path]())
      .iterator()
      .asScala
      .foreach(Files.delete)
  }

  private def prepareGraph(): Unit = {
    val t = underTest
    import t._

    NewBlockPackage.Genesis.modifications.run().futureRightValue

    Source
      .unfold(NewBlockPackage.Genesis) { parentBlockPackage =>
        val nextBlock = parentBlockPackage.nextBlockPackage
        Some((nextBlock, nextBlock))
      }
      .mapAsync(1)(_.modifications.run().value.map(_.value))
      .take(count - 1)
      .runWith(Sink.ignore)
      .futureValue(Timeout(Duration.Inf))
  }
}

case class NewBlockPackage(
  parentBlockId:       Option[String],
  header:              BlockHeader,
  body:                BlockBody,
  transactionPackages: List[NewTransactionPackage]
) {

  def nextBlockPackage: NewBlockPackage = {
    val newBlockId = s"${header.height + 1}_1"
    NewBlockPackage(
      parentBlockId = Some(header.blockId),
      header = BlockHeader(
        blockId = newBlockId,
        timestamp = header.timestamp + 1,
        publicKey = "topl",
        signature = "topl",
        height = header.height + 1,
        difficulty = 1,
        txRoot = "topl",
        bloomFilter = "topl",
        version = 1
      ),
      body = BlockBody(
        blockId = newBlockId
      ),
      transactionPackages = List(
        NewTransactionPackage(
          Transaction(
            newBlockId + "_1",
            "0",
            header.timestamp + 1,
            data = None,
            minting = true,
            attestation = Map("topl" -> "BobSaidSo")
          ),
          openedBoxIds = List(header.blockId + "_1"),
          newBoxes = List(
            Box(
              newBlockId + "_1",
              boxType = 1,
              value = "1",
              nonce = 1
            ),
            Box(
              newBlockId + "_2",
              boxType = 1,
              value = "1",
              nonce = 1
            )
          )
        )
      )
    )
  }

  def modifications: NonEmptyChain[BlockchainModification] =
    NonEmptyChain
      .fromSeq(
        List(
          CreateBlockHeader(header),
          CreateBlockBody(body),
          AssociateBodyToHeader(body.blockId, header.blockId),
          SetHead(header.blockId)
        ) ++ parentBlockId.map(AssociateBlockToParent(header.blockId, _)) ++ transactionPackages.flatMap(t =>
          List(
            CreateTransaction(t.transaction),
            AssociateTransactionToBody(t.transaction.transactionId, body.blockId, index = 0)
          ) ++ t.openedBoxIds.flatMap(id => List(AssociateBoxOpener(id, t.transaction.transactionId))) ++ t.newBoxes
            .flatMap(newBox =>
              List(CreateBox(newBox), AssociateBoxCreator(newBox.boxId, t.transaction.transactionId, minted = false))
            )
        )
      )
      .get

}

object NewBlockPackage {

  val Genesis: NewBlockPackage = NewBlockPackage(
    parentBlockId = None,
    header = BlockHeader(
      "1_1",
      timestamp = 1L,
      publicKey = "topl",
      signature = "topl",
      height = 1,
      difficulty = 1,
      txRoot = "topl",
      bloomFilter = "topl",
      version = 1
    ),
    body = BlockBody("1_1"),
    transactionPackages = List(
      NewTransactionPackage(
        Transaction(
          "1_1_1",
          "0",
          0,
          data = None,
          minting = true,
          attestation = Map("topl" -> "BobSaidSo")
        ),
        openedBoxIds = Nil,
        newBoxes = List(
          Box(
            "1_1_1",
            boxType = 1,
            value = "1000000",
            nonce = 1
          )
        )
      )
    )
  )
}

case class NewTransactionPackage(transaction: Transaction, openedBoxIds: List[String], newBoxes: List[Box])

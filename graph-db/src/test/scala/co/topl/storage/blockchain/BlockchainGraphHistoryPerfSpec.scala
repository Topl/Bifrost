package co.topl.storage.blockchain

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import akka.stream.scaladsl.{Sink, Source}
import cats.data.{Chain, NonEmptyChain}
import cats.scalatest.FutureEitherValues
import co.topl.storage.graph.OrientDBGraph
import co.topl.storage.leveldb.LevelDBStore
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
class BlockchainGraphHistoryPerfSpec
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
  private var genericDb: LevelDBStore = _
  private var underTest: BlockchainGraph = _

  private val count = 5000
  private val parallelism = 2

  it should "retrieve a long history" in {
    val t = underTest
    import t._

    val head = Blockchain.currentHead.futureRightValue

    head.blockId shouldBe s"block$count"

    logger.info("Traversing history")

    val (totalBlocks, genesisParentIndex) =
      head.history
        .map(_.value)
        .runFold((0, count - 1)) { case ((totalCount, height), header) =>
          header.blockId shouldBe s"block$height"
          (totalCount + 1, height - 1)
        }
        .futureValue(Timeout(10.minutes))

    logger.info("Finished traversing history")

    totalBlocks shouldBe count
    genesisParentIndex shouldBe -1
  }

  override def beforeAll(): Unit = {
    super.beforeAll()

    val schema = BlockchainGraphSchema.value

    dataDir = Paths.get(".", "target", "test", "db" + System.currentTimeMillis().toString)

    graph = OrientDBGraph(schema, OrientDBGraph.Local(Paths.get(dataDir.toString, "graph")))
//    graph = OrientDBGraph(schema, OrientDBGraph.InMemory)

    genericDb = new LevelDBStore(Paths.get(dataDir.toString, "genericdb"))

    underTest = new BlockchainGraph(graph, parallelism)(system, genericDb)

    prepareGraph()
  }

  override def afterAll(): Unit = {
    underTest.close()
    super.afterAll()

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

    logger.info(s"Inserting $count block headers")

    Source(0 to count)
      .grouped(10)
      .map(indices =>
        NonEmptyChain
          .fromChainUnsafe(Chain.fromSeq(indices))
          .map(i =>
            CreateBlockHeader(
              BlockHeader(
                blockId = s"block$i",
                timestamp = i,
                publicKey = "topl",
                signature = "topl",
                height = i,
                difficulty = 1,
                txRoot = "bar",
                bloomFilter = "baz",
                version = 1
              )
            )
          )
      )
      .runWith(Sink.foreachAsync(parallelism)(v => v.run().value.map(_.value)))
      .futureValue(Timeout(10.minutes))

    logger.info("Inserting block header parent edges")

    Source(0 until parallelism)
      .flatMapMerge(
        parallelism,
        partition =>
          Source((((count / parallelism) * partition) + 1) to ((count / parallelism) * (partition + 1)))
            .grouped(10)
            .map(indices =>
              NonEmptyChain
                .fromChainUnsafe(Chain.fromSeq(indices))
                .map(i => AssociateBlockToParent(s"block$i", s"block${i - 1}"))
            )
            .mapAsync(1)(v => v.run().value.map(_.value))
      )
      .runWith(Sink.ignore)
      .futureValue(Timeout(10.minutes))

    NonEmptyChain(SetHead(s"block$count")).run().futureRightValue

    logger.info("Finished inserting headers and edges")

  }
}

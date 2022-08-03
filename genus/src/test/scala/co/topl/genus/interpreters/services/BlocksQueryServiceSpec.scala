package co.topl.genus.interpreters.services

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import co.topl.genus.algebras.ChainHeight
import co.topl.genus.algebras.QueryService.QueryRequest
import co.topl.genus.filters.BlockFilter
import co.topl.genus.interpreters.{MockChainHeight, MockMongoStore}
import co.topl.genus.ops.EitherTSourceOps.implicits._
import co.topl.genus.services.blocks_query.BlockSorting
import co.topl.genus.typeclasses.implicits._
import co.topl.genus.types.Block
import co.topl.utils.mongodb.codecs._
import co.topl.utils.mongodb.implicits._
import co.topl.utils.mongodb.models._
import org.mongodb.scala.Document
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AsyncFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class BlocksQueryServiceSpec
    extends ScalaTestWithActorTestKit
    with AsyncFlatSpecLike
    with Matchers
    with ScalaCheckDrivenPropertyChecks
    with MockFactory {

  import BlocksQueryServiceSpec._

  behavior of "Blocks Query Service"

  it should "return all blocks that mongo provides" in {
    val blocks = List(testBlockModel, testBlockModel)

    val dataStore = MockMongoStore.withBlocks[IO](blocks)
    implicit val chainHeight: ChainHeight[IO] = MockChainHeight.default[IO]

    val underTest = BlocksQueryService.make[IO](dataStore)

    val result = underTest.query(defaultQueryRequest).materializeToList

    result.value
      .map(values => values shouldBe Right(blocks.map(_.transformTo[Block])))
      .unsafeToFuture()
  }

  it should "not return documents that fail to convert into blocks" in {
    val block = testBlockModel

    val validBlockDocuments = List(block.asDocument)
    val invalidBlockDocuments = List(Document("{ \"invalid\": \"test\" }"))

    val dataStore = MockMongoStore.withDocuments[IO](validBlockDocuments ++ invalidBlockDocuments)
    implicit val chainHeight: ChainHeight[IO] = MockChainHeight.default[IO]

    val underTest = BlocksQueryService.make[IO](dataStore)

    val result =
      underTest
        .query(defaultQueryRequest)
        .materializeToList

    result.value
      .map(values => values shouldBe Right(List(block.transformTo[Block])))
      .unsafeToFuture()
  }
}

object BlocksQueryServiceSpec {

  def testBlockModel: BlockDataModel =
    BlockDataModel(
      "test-block-id",
      "test-block-parent-id",
      "10000",
      TokenBoxDataModel("test-type", "test-id", "1888", "test-evidence", SimpleValueDataModel("9999")),
      "test-public-key",
      "test-signature",
      0,
      "test-difficulty",
      "test-tx-root",
      "test-bloom-filter",
      1,
      1000,
      555,
      "test-fees"
    )

  val defaultQueryRequest: QueryRequest[BlockFilter, BlockSorting] =
    QueryRequest[BlockFilter, BlockSorting](
      BlockFilter.defaultInstance,
      BlockSorting.defaultInstance,
      None,
      0
    )
}

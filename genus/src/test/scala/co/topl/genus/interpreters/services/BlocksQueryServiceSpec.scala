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
import co.topl.genus.types.{Block, BlockHeight}
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

  it should "not return blocks with heights that are above the confirmation depth" in {
    val confirmationDepth = 8
    val currentChainHeight = 100

    val dataStore = MockMongoStore.withBlocks[IO](List(createBlockWithHeight(100)))
    implicit val chainHeight: ChainHeight[IO] = MockChainHeight.withHeight[IO](BlockHeight(currentChainHeight))

    val underTest = BlocksQueryService.make[IO](dataStore)

    val result =
      underTest
        .query(queryRequestWithConfirmationDepth(confirmationDepth))
        .materializeToList

    result.value
      .map(values => values shouldBe Right(List.empty[Block]))
      .unsafeToFuture()
  }

  it should "return all blocks mongo provides when confirmation depth is 0" in {
    val confirmationDepth = 0
    val currentChainHeight = 100

    val blocks =
      List(
        createBlockWithHeight(33),
        createBlockWithHeight(66)
      )

    val dataStore = MockMongoStore.withBlocks[IO](blocks)
    implicit val chainHeight: ChainHeight[IO] = MockChainHeight.withHeight[IO](BlockHeight(currentChainHeight))

    val underTest = BlocksQueryService.make[IO](dataStore)

    val result =
      underTest
        .query(queryRequestWithConfirmationDepth(confirmationDepth))
        .materializeToList

    result.value
      .map(values => values shouldBe Right(blocks.map(_.transformTo[Block])))
      .unsafeToFuture()
  }

  it should "not return documents that fail to convert into blocks" in {
    val confirmationDepth = 0
    val currentChainHeight = 100

    val block = createBlockWithHeight(77)

    val validBlockDocuments = List(block.asDocument)
    val invalidBlockDocuments = List(Document("{ \"invalid\": \"test\" }"))

    val dataStore = MockMongoStore.withDocuments[IO](validBlockDocuments ++ invalidBlockDocuments)
    implicit val chainHeight: ChainHeight[IO] = MockChainHeight.withHeight[IO](BlockHeight(currentChainHeight))

    val underTest = BlocksQueryService.make[IO](dataStore)

    val result =
      underTest
        .query(queryRequestWithConfirmationDepth(confirmationDepth))
        .materializeToList

    result.value
      .map(values => values shouldBe Right(List(block.transformTo[Block])))
      .unsafeToFuture()
  }
}

object BlocksQueryServiceSpec {

  def createBlockWithHeight(height: Long): BlockDataModel =
    BlockDataModel(
      "test-block-id",
      "test-block-parent-id",
      "10000",
      TokenBoxDataModel("test-type", "test-id", "1888", "test-evidence", SimpleValueDataModel("9999")),
      "test-public-key",
      "test-signature",
      height,
      "test-difficulty",
      "test-tx-root",
      "test-bloom-filter",
      1,
      1000,
      555,
      "test-fees"
    )

  def queryRequestWithConfirmationDepth(depth: Int): QueryRequest[BlockFilter, BlockSorting] =
    QueryRequest(
      BlockFilter.defaultInstance,
      BlockSorting.defaultInstance,
      None,
      depth
    )
}

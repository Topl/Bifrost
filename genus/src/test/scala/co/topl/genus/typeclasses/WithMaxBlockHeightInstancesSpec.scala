package co.topl.genus.typeclasses

import cats.Id
import co.topl.genus.algebras.ChainHeight
import co.topl.genus.filters.{BlockFilter, StringSelection, TransactionFilter}
import co.topl.genus.interpreters.MockChainHeight
import org.scalatest.flatspec.AnyFlatSpec
import co.topl.genus.typeclasses.implicits._
import co.topl.genus.types.BlockHeight
import org.scalatest.matchers.should.Matchers

class WithMaxBlockHeightInstancesSpec extends AnyFlatSpec with Matchers {
  behavior of "Transaction Filter With Max Block Height"

  it should "combine the max block height filter using And" in {
    val initialFilter =
      TransactionFilter(TransactionFilter.FilterType.TxIdSelection(StringSelection(Seq("test-tx-id"))))

    val currentChainHeight = 100001

    val expectedBson =
      """{"$and": [{"txId": {"$in": ["test-tx-id"]}}, {"block.height": {"$lte": 100001}}]}"""

    val result = initialFilter.withMaxBlockHeight(BlockHeight(currentChainHeight))

    result.toBsonFilter.toBsonDocument.toJson shouldBe expectedBson
  }

  it should "combine the max block value using And when using withConfirmationDepth" in {
    val initialFilter =
      TransactionFilter(TransactionFilter.FilterType.TxIdSelection(StringSelection(Seq("test-tx-id"))))

    val currentChainHeight = 55
    val confirmationDepth = 3

    implicit val chainHeight: ChainHeight[Id] = MockChainHeight.withHeight(BlockHeight(currentChainHeight))

    val expectedBson =
      """{"$and": [{"txId": {"$in": ["test-tx-id"]}}, {"block.height": {"$lte": 52}}]}"""

    val result = initialFilter.withConfirmationDepth[Id](confirmationDepth)

    result.toBsonFilter.toBsonDocument.toJson shouldBe expectedBson
  }

  behavior of "Block Filter With Max Block Height"

  it should "combine the max block height filter using And" in {
    val initialFilter =
      BlockFilter(BlockFilter.FilterType.IdSelection(StringSelection(Seq("test-block-id"))))

    val currentChainHeight = 89997

    val expectedBson =
      """{"$and": [{"id": {"$in": ["test-block-id"]}}, {"height": {"$lte": 89997}}]}"""

    val result = initialFilter.withMaxBlockHeight(BlockHeight(currentChainHeight))

    result.toBsonFilter.toBsonDocument.toJson shouldBe expectedBson
  }

  it should "combine the max block value using And when using withConfirmationDepth" in {
    val initialFilter =
      BlockFilter(BlockFilter.FilterType.IdSelection(StringSelection(Seq("test-block-id"))))

    val currentChainHeight = 77
    val confirmationDepth = 10

    implicit val chainHeight: ChainHeight[Id] = MockChainHeight.withHeight(BlockHeight(currentChainHeight))

    val expectedBson =
      """{"$and": [{"id": {"$in": ["test-block-id"]}}, {"height": {"$lte": 67}}]}"""

    val result = initialFilter.withConfirmationDepth[Id](confirmationDepth)

    result.toBsonFilter.toBsonDocument.toJson shouldBe expectedBson
  }
}

package co.topl.genus

import cats.~>
import co.topl.genus.filters.{BlockFilter, TransactionFilter}
import org.bson.conversions.Bson
import org.mongodb.scala.model.Sorts

import scala.concurrent.Future

package object interpreters {
  val defaultBlockFilter: BlockFilter = BlockFilter.of(BlockFilter.FilterType.All(BlockFilter.AllFilter()))

  val defaultTransactionFilter: TransactionFilter =
    TransactionFilter.of(TransactionFilter.FilterType.All(TransactionFilter.AllFilter()))

  val defaultBlockSort: Bson = Sorts.ascending("height")

  val defaultTransactionSort: Bson = Sorts.ascending("block.height")
}

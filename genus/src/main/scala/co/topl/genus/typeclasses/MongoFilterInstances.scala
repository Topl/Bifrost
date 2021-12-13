package co.topl.genus.typeclasses

import co.topl.genus.filters.{BlockFilter, TransactionFilter}
import org.mongodb.scala.model.Filters

trait MongoFilterInstances {
  implicit val transactionMongoFilter: MongoFilter[TransactionFilter] = _ => Filters.empty()

  implicit val blockMongoFilter: MongoFilter[BlockFilter] = _ => Filters.empty()
}

package co.topl.genus.typeclasses.mongofilter

import co.topl.genus.Generators
import co.topl.genus.filters.TransactionFilter
import co.topl.genus.typeclasses.implicits._

class TransactionMongoFilterSpec extends MongoFilterBehavior {
  testMongoFilterBehavior[TransactionFilter]("Transaction Filter", Generators.transactionFilterGen)
}

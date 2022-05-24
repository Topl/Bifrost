package co.topl.genus.typeclasses.mongofilter

import co.topl.genus.ArbitraryInstances
import co.topl.genus.filters.TransactionFilter
import co.topl.genus.typeclasses.implicits._

class TransactionMongoFilterSpec extends MongoFilterBehavior with ArbitraryInstances {
  testMongoFilterBehavior[TransactionFilter]("Transaction Filter")
}

package co.topl.genus.typeclasses.mongofilter

import co.topl.genus.ArbitraryInstances
import co.topl.genus.filters.BlockFilter
import co.topl.genus.typeclasses.implicits._

class BlockMongoFilterSpec extends MongoFilterBehavior with ArbitraryInstances {
  testMongoFilterBehavior[BlockFilter]("Block Filter")
}

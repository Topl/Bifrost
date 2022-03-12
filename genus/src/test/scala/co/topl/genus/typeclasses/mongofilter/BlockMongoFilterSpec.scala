package co.topl.genus.typeclasses.mongofilter

import co.topl.genus.Generators
import co.topl.genus.filters.BlockFilter
import co.topl.genus.typeclasses.implicits._

class BlockMongoFilterSpec extends MongoFilterBehavior {
  testMongoFilterBehavior[BlockFilter]("Block Filter", Generators.blockFilterGen)
}

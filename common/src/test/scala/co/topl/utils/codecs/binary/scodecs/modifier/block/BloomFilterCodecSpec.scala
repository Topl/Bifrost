package co.topl.utils.codecs.binary.scodecs.modifier.block

import cats.{Eq, Show}
import co.topl.modifier.block.BloomFilter
import co.topl.utils.CommonGenerators
import co.topl.utils.codecs.binary.CodecCompatabilityBehavior
import co.topl.utils.codecs.binary.legacy.modifier.block.BloomFilterSerializer
import co.topl.utils.codecs.binary.scodecs.modifier.block._

class BloomFilterCodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  implicit private val eq: Eq[BloomFilter] = Eq.fromUniversalEquals
  implicit private val show: Show[BloomFilter] = Show.fromToString

  codecCompatabilityBehavior(
    "bloom filter",
    bloomFilterCodec,
    BloomFilterSerializer,
    bloomFilterGen
  )
}

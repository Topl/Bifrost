package co.topl.codecs.binary.scodecs.modifier.block

import co.topl.codecs.binary.CodecCompatabilityBehavior
import co.topl.codecs.binary.legacy.modifier.block.BloomFilterSerializer
import co.topl.utils.CommonGenerators
import co.topl.utils.catsInstances._

class BloomFilterCodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  codecCompatabilityBehavior(
    "bloom filter",
    bloomFilterCodec,
    BloomFilterSerializer,
    bloomFilterGen
  )
}

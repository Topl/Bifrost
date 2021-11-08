package co.topl.codecs.binary.scodecs.modifier.block

import co.topl.codecs.binary.CodecCompatabilityBehavior
import co.topl.codecs.binary.legacy.modifier.block.BlockHeaderSerializer
import co.topl.utils.CommonGenerators
import co.topl.utils.catsInstances._

class BlockHeaderCodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  codecCompatabilityBehavior(
    "block header",
    blockHeaderCodec,
    BlockHeaderSerializer,
    blockCurve25519Gen.map(_.toComponents._1)
  )
}

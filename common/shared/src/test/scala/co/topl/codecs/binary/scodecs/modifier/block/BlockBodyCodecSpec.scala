package co.topl.codecs.binary.scodecs.modifier.block

import co.topl.codecs.binary.CodecCompatabilityBehavior
import co.topl.codecs.binary.legacy.modifier.block.BlockBodySerializer
import co.topl.utils.CommonGenerators
import co.topl.utils.implicits._

class BlockBodyCodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  codecCompatabilityBehavior(
    "block body",
    blockBodyCodec,
    BlockBodySerializer,
    blockCurve25519Gen.map(_.toComponents._2)
  )
}

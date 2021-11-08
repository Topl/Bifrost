package co.topl.codecs.binary.scodecs.modifier.block

import co.topl.codecs.binary.CodecCompatabilityBehavior
import co.topl.codecs.binary.legacy.modifier.block.BlockSerializer
import co.topl.utils.CommonGenerators
import co.topl.utils.catsInstances._

class BlockCodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  codecCompatabilityBehavior("block", blockCodec, BlockSerializer, blockCurve25519Gen)
}

package co.topl.utils.codecs.binary.scodecs.modifier.block

import cats.{Eq, Show}
import co.topl.modifier.block.Block
import co.topl.utils.CommonGenerators
import co.topl.utils.codecs.binary.CodecCompatabilityBehavior
import co.topl.utils.codecs.binary.legacy.modifier.block.BlockSerializer

class BlockCodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  implicit private val eq: Eq[Block] = Eq.fromUniversalEquals
  implicit private val show: Show[Block] = Show.fromToString

  codecCompatabilityBehavior("block", blockCodec, BlockSerializer, blockCurve25519Gen)
}

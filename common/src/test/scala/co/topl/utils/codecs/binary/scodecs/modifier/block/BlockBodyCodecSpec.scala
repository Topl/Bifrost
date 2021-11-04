package co.topl.utils.codecs.binary.scodecs.modifier.block

import cats.{Eq, Show}
import co.topl.modifier.block.BlockBody
import co.topl.utils.CommonGenerators
import co.topl.utils.codecs.binary.CodecCompatabilityBehavior
import co.topl.utils.codecs.binary.legacy.modifier.block.BlockBodySerializer

class BlockBodyCodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  implicit private val eq: Eq[BlockBody] = Eq.fromUniversalEquals
  implicit private val show: Show[BlockBody] = Show.fromToString

  codecCompatabilityBehavior(
    "block body",
    blockBodyCodec,
    BlockBodySerializer,
    blockCurve25519Gen.map(_.toComponents._2)
  )
}

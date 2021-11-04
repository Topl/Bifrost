package co.topl.utils.codecs.binary.scodecs.modifier.box

import cats.{Eq, Show}
import co.topl.modifier.box.AssetBox
import co.topl.utils.CommonGenerators
import co.topl.utils.codecs.binary.CodecCompatabilityBehavior
import co.topl.utils.codecs.binary.legacy.modifier.box.AssetBoxSerializer
import co.topl.utils.codecs.binary.scodecs.modifier.box._

class AssetBoxCodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  implicit private val eq: Eq[AssetBox] = Eq.fromUniversalEquals
  implicit private val show: Show[AssetBox] = Show.fromToString

  codecCompatabilityBehavior("asset box", assetBoxCodec, AssetBoxSerializer, assetBoxGen)
}

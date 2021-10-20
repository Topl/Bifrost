package co.topl.utils.codecs.binary.modifier.box

import cats.{Eq, Show}
import co.topl.modifier.box.AssetValue
import co.topl.utils.CommonGenerators
import co.topl.utils.codecs.binary.CodecCompatabilityBehavior
import co.topl.utils.codecs.binary.legacy.modifier.box.AssetValueSerializer
import co.topl.utils.codecs.binary.modifier.box.codecs._

class AssetValueCodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  implicit private val eq: Eq[AssetValue] = Eq.fromUniversalEquals
  implicit private val show: Show[AssetValue] = Show.fromToString

  codecCompatabilityBehavior("asset value", assetValueCodec, AssetValueSerializer, assetValueGen)
}

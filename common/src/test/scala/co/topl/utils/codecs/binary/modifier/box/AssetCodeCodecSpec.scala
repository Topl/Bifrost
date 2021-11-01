package co.topl.utils.codecs.binary.modifier.box

import cats.{Eq, Show}
import co.topl.modifier.box.{AssetBox, AssetCode}
import co.topl.utils.CommonGenerators
import co.topl.utils.codecs.binary.CodecCompatabilityBehavior
import co.topl.utils.codecs.binary.legacy.modifier.box.AssetCodeSerializer
import co.topl.utils.codecs.binary.modifier.box.codecs._

class AssetCodeCodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  implicit private val eq: Eq[AssetCode] = Eq.fromUniversalEquals
  implicit private val show: Show[AssetCode] = Show.fromToString

  codecCompatabilityBehavior("asset code", assetCodeCodec, AssetCodeSerializer, assetCodeGen)
}

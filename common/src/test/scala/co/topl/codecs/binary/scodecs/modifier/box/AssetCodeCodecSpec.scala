package co.topl.codecs.binary.scodecs.modifier.box

import co.topl.codecs.binary.CodecCompatabilityBehavior
import co.topl.codecs.binary.legacy.modifier.box.AssetCodeSerializer
import co.topl.utils.CommonGenerators
import co.topl.utils.implicits._

class AssetCodeCodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  codecCompatabilityBehavior("asset code", assetCodeCodec, AssetCodeSerializer, assetCodeGen)
}

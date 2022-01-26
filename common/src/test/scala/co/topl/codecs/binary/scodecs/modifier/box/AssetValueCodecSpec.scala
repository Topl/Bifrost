package co.topl.codecs.binary.scodecs.modifier.box

import co.topl.codecs.binary.CodecCompatabilityBehavior
import co.topl.codecs.binary.legacy.modifier.box.AssetValueSerializer
import co.topl.utils.CommonGenerators
import co.topl.utils.catsInstances._

class AssetValueCodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  codecCompatabilityBehavior("asset value", assetValueCodec, AssetValueSerializer, assetValueGen)
}

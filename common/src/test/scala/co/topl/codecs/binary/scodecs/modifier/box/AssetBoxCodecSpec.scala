package co.topl.codecs.binary.scodecs.modifier.box

import co.topl.codecs.binary.CodecCompatabilityBehavior
import co.topl.codecs.binary.legacy.modifier.box.AssetBoxSerializer
import co.topl.utils.CommonGenerators
import co.topl.utils.catsInstances._

class AssetBoxCodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  codecCompatabilityBehavior("asset box", assetBoxCodec, AssetBoxSerializer, assetBoxGen)
}

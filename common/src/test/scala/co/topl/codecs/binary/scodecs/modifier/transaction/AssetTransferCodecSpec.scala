package co.topl.codecs.binary.scodecs.modifier.transaction

import co.topl.codecs.binary.CodecCompatabilityBehavior
import co.topl.codecs.binary.legacy.modifier.transaction.AssetTransferSerializer
import co.topl.utils.CommonGenerators
import co.topl.utils.catsInstances._

class AssetTransferCodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  codecCompatabilityBehavior("asset transfer", assetTransferCodec, AssetTransferSerializer, assetTransferGen)
}

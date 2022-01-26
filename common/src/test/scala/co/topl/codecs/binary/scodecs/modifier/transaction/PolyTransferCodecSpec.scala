package co.topl.codecs.binary.scodecs.modifier.transaction

import co.topl.codecs.binary.CodecCompatabilityBehavior
import co.topl.codecs.binary.legacy.modifier.transaction.PolyTransferSerializer
import co.topl.utils.CommonGenerators
import co.topl.utils.catsInstances._

class PolyTransferCodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  codecCompatabilityBehavior("poly transfer", polyTransferCodec, PolyTransferSerializer, polyTransferGen)
}

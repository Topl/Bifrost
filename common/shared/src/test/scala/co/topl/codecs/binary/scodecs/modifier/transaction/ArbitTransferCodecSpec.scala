package co.topl.codecs.binary.scodecs.modifier.transaction

import co.topl.codecs.binary.CodecCompatabilityBehavior
import co.topl.codecs.binary.legacy.modifier.transaction.ArbitTransferSerializer
import co.topl.utils.CommonGenerators
import co.topl.utils.implicits._

class ArbitTransferCodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  codecCompatabilityBehavior("arbit transfer", arbitTransferCodec, ArbitTransferSerializer, arbitTransferGen)
}

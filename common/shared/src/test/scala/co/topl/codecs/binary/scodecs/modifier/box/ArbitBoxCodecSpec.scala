package co.topl.codecs.binary.scodecs.modifier.box

import co.topl.codecs.binary.CodecCompatabilityBehavior
import co.topl.codecs.binary.legacy.modifier.box.ArbitBoxSerializer
import co.topl.utils.CommonGenerators
import co.topl.utils.implicits._

class ArbitBoxCodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  codecCompatabilityBehavior("arbit box", arbitBoxCodec, ArbitBoxSerializer, arbitBoxGen)
}

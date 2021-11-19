package co.topl.codecs.binary.scodecs.modifier.box

import co.topl.codecs.binary.CodecCompatabilityBehavior
import co.topl.codecs.binary.legacy.modifier.box.PolyBoxSerializer
import co.topl.utils.CommonGenerators
import co.topl.utils.catsInstances._

class PolyBoxCodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  codecCompatabilityBehavior("poly box", polyBoxCodec, PolyBoxSerializer, polyBoxGen)
}

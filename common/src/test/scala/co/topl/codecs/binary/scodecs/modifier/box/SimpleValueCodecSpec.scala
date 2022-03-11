package co.topl.codecs.binary.scodecs.modifier.box

import co.topl.codecs.binary.CodecCompatabilityBehavior
import co.topl.codecs.binary.legacy.modifier.box.SimpleValueSerializer
import co.topl.utils.CommonGenerators
import co.topl.utils.implicits._

class SimpleValueCodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  codecCompatabilityBehavior("simple value", simpleValueCodec, SimpleValueSerializer, simpleValueGen)
}

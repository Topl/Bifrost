package co.topl.codecs.binary.scodecs.modifier

import co.topl.codecs.binary.CodecCompatabilityBehavior
import co.topl.codecs.binary.legacy.modifier.ModifierIdSerializer
import co.topl.utils.CommonGenerators
import co.topl.utils.catsInstances._

class ModifierIdCodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  codecCompatabilityBehavior("modifier ID", modifierIdCodec, ModifierIdSerializer, modifierIdGen)
}

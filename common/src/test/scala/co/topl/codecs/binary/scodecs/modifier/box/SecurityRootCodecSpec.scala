package co.topl.codecs.binary.scodecs.modifier.box

import co.topl.codecs.binary.CodecCompatabilityBehavior
import co.topl.codecs.binary.legacy.modifier.box.SecurityRootSerializer
import co.topl.utils.CommonGenerators
import co.topl.utils.implicits._

class SecurityRootCodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  codecCompatabilityBehavior("security root", securityRootCodec, SecurityRootSerializer, securityRootGen)
}

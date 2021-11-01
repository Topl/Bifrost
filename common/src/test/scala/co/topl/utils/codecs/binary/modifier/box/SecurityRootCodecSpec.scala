package co.topl.utils.codecs.binary.modifier.box

import cats.{Eq, Show}
import co.topl.modifier.box.SecurityRoot
import co.topl.utils.CommonGenerators
import co.topl.utils.codecs.binary.CodecCompatabilityBehavior
import co.topl.utils.codecs.binary.legacy.modifier.box.SecurityRootSerializer
import co.topl.utils.codecs.binary.modifier.box.codecs._

class SecurityRootCodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  implicit private val eq: Eq[SecurityRoot] = Eq.fromUniversalEquals
  implicit private val show: Show[SecurityRoot] = Show.fromToString

  codecCompatabilityBehavior("security root", securityRootCodec, SecurityRootSerializer, securityRootGen)
}

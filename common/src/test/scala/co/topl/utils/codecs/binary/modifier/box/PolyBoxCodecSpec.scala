package co.topl.utils.codecs.binary.modifier.box

import cats.{Eq, Show}
import co.topl.modifier.box.PolyBox
import co.topl.utils.CommonGenerators
import co.topl.utils.codecs.binary.CodecCompatabilityBehavior
import co.topl.utils.codecs.binary.legacy.modifier.box.PolyBoxSerializer
import co.topl.utils.codecs.binary.modifier.box.codecs._

class PolyBoxCodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  implicit private val eq: Eq[PolyBox] = Eq.fromUniversalEquals
  implicit private val show: Show[PolyBox] = Show.fromToString

  codecCompatabilityBehavior("poly box", polyBoxCodec, PolyBoxSerializer, polyBoxGen)
}

package co.topl.utils.codecs.binary.scodecs.modifier.box

import cats.{Eq, Show}
import co.topl.modifier.box.ArbitBox
import co.topl.utils.CommonGenerators
import co.topl.utils.codecs.binary.CodecCompatabilityBehavior
import co.topl.utils.codecs.binary.legacy.modifier.box.ArbitBoxSerializer
import co.topl.utils.codecs.binary.scodecs.modifier.box._

class ArbitBoxCodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  implicit private val eq: Eq[ArbitBox] = Eq.fromUniversalEquals
  implicit private val show: Show[ArbitBox] = Show.fromToString

  codecCompatabilityBehavior("arbit box", arbitBoxCodec, ArbitBoxSerializer, arbitBoxGen)
}

package co.topl.utils.codecs.binary.modifier.box

import cats.{Eq, Show}
import co.topl.modifier.box.{SecurityRoot, SimpleValue}
import co.topl.utils.CommonGenerators
import co.topl.utils.codecs.binary.CodecCompatabilityBehavior
import co.topl.utils.codecs.binary.legacy.modifier.box.SimpleValueSerializer
import co.topl.utils.codecs.binary.modifier.box.codecs._

class SimpleValueCodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  implicit private val eq: Eq[SimpleValue] = Eq.fromUniversalEquals
  implicit private val show: Show[SimpleValue] = Show.fromToString

  codecCompatabilityBehavior("simple value", simpleValueCodec, SimpleValueSerializer, simpleValueGen)
}

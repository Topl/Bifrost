package co.topl.utils.codecs.binary.scodecs.modifier.box

import cats.{Eq, Show}
import co.topl.modifier.box.Box
import co.topl.utils.CommonGenerators
import co.topl.utils.codecs.binary.CodecCompatabilityBehavior
import co.topl.utils.codecs.binary.legacy.modifier.box.BoxSerializer
import co.topl.utils.codecs.binary.scodecs.modifier.box._
import org.scalacheck.Gen

class BoxCodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  implicit private val eq: Eq[Box[_]] = Eq.fromUniversalEquals
  implicit private val show: Show[Box[_]] = Show.fromToString

  codecCompatabilityBehavior("box", boxCodec, BoxSerializer, Gen.oneOf[Box[_]](assetBoxGen, arbitBoxGen, polyBoxGen))
}

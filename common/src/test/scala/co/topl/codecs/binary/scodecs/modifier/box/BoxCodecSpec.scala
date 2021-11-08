package co.topl.codecs.binary.scodecs.modifier.box

import cats.{Eq, Show}
import co.topl.modifier.box.Box
import co.topl.utils.CommonGenerators
import co.topl.codecs.binary.CodecCompatabilityBehavior
import co.topl.codecs.binary.legacy.modifier.box.BoxSerializer
import co.topl.codecs.binary.scodecs.modifier.box._
import org.scalacheck.Gen
import co.topl.utils.catsInstances._

class BoxCodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  codecCompatabilityBehavior("box", boxCodec, BoxSerializer, Gen.oneOf[Box[_]](assetBoxGen, arbitBoxGen, polyBoxGen))
}

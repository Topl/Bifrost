package co.topl.codecs.binary.scodecs.modifier.box

import co.topl.codecs.binary.CodecCompatabilityBehavior
import co.topl.codecs.binary.legacy.modifier.box.BoxSerializer
import co.topl.modifier.box.Box
import co.topl.utils.CommonGenerators
import co.topl.utils.catsInstances._
import org.scalacheck.Gen

class BoxCodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  codecCompatabilityBehavior("box", boxCodec, BoxSerializer, Gen.oneOf[Box[_]](assetBoxGen, arbitBoxGen, polyBoxGen))
}

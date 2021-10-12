package co.topl.utils.codecs.binary.attestation

import co.topl.attestation.Evidence
import scodec.Codec
import co.topl.utils.codecs.binary.valuetypes.codecs.bytes

object EvidenceCodec {
  val codec: Codec[Evidence] = bytes(Evidence.size).as[Evidence]

  trait Codecs {
    val evidence: Codec[Evidence] = codec
  }

  trait Implicits {
    implicit val implicitEvidence: Codec[Evidence] = codec
  }

  object codecs extends Codecs
  object implicits extends Implicits
}

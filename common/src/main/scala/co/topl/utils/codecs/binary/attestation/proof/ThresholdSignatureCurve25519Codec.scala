package co.topl.utils.codecs.binary.attestation.proof

import co.topl.utils.codecs.binary.valuetypes.implicits._
import co.topl.utils.codecs.binary.valuetypes.codecs._
import co.topl.attestation.{SignatureCurve25519, ThresholdSignatureCurve25519}
import scodec.Codec
import co.topl.utils.codecs.binary.attestation.proof.SignatureCurve25519Codec.implicits._

object ThresholdSignatureCurve25519Codec {

  val codec: Codec[ThresholdSignatureCurve25519] =
    list[SignatureCurve25519]
      .as[Set[SignatureCurve25519]]
      .as[ThresholdSignatureCurve25519]

  trait Codecs {
    val thresholdSignatureCurve25519: Codec[ThresholdSignatureCurve25519] = codec
  }

  trait Implicits {
    implicit val implicitThresholdSignatureCurve25519: Codec[ThresholdSignatureCurve25519] = codec
  }

  object codecs extends Codecs
  object implicits extends Implicits
}

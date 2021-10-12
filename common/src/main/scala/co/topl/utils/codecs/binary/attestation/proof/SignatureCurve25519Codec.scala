package co.topl.utils.codecs.binary.attestation.proof

import co.topl.attestation.SignatureCurve25519
import co.topl.utils.codecs.binary.crypto.SignatureCodec.codecs._
import scodec.Codec

object SignatureCurve25519Codec {

  val codec: Codec[SignatureCurve25519] = signature(SignatureCurve25519.signatureSize).as[SignatureCurve25519]

  trait Codecs {
    val signatureCurve25519: Codec[SignatureCurve25519] = codec
  }

  trait Implicits {
    implicit val implicitSignatureCurve25519: Codec[SignatureCurve25519] = codec
  }

  object codecs extends Codecs
  object implicits extends Implicits
}

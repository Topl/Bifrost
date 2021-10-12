package co.topl.utils.codecs.binary.attestation.proof

import co.topl.attestation.SignatureEd25519
import scodec.Codec
import co.topl.utils.codecs.binary.crypto.SignatureCodec.codecs._

object SignatureEd25519Codec {
  val codec: Codec[SignatureEd25519] = signature(SignatureEd25519.signatureSize).as[SignatureEd25519]

  trait Codecs {
    val signatureEd25519: Codec[SignatureEd25519] = codec
  }

  trait Implicits {
    implicit val implicitSignatureEd25519: Codec[SignatureEd25519] = codec
  }

  object codecs extends Codecs
  object implicits extends Implicits
}

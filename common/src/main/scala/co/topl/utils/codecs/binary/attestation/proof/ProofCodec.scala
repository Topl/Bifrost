package co.topl.utils.codecs.binary.attestation.proof

import co.topl.utils.codecs.binary.attestation.proof.SignatureCurve25519Codec.codecs._
import co.topl.utils.codecs.binary.attestation.proof.ThresholdSignatureCurve25519Codec.codecs._
import co.topl.utils.codecs.binary.attestation.proof.SignatureEd25519Codec.codecs._
import co.topl.utils.codecs.binary.valuetypes.codecs._
import co.topl.attestation._
import scodec.Codec
import scodec.codecs.discriminated

object ProofCodec {

  val codec: Codec[Proof[_]] =
    discriminated[Proof[_]]
      .by(byte)
      .typecase(PublicKeyPropositionCurve25519.typePrefix, signatureCurve25519)
      .typecase(ThresholdPropositionCurve25519.typePrefix, thresholdSignatureCurve25519)
      .typecase(PublicKeyPropositionEd25519.typePrefix, signatureEd25519)

  trait Codecs {
    val proof: Codec[Proof[_]] = codec
  }

  trait Implicits {
    implicit val implicitProof: Codec[Proof[_]] = codec
  }

  object codecs extends Codecs
  object implicits extends Implicits
}

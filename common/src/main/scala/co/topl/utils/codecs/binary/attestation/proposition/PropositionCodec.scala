package co.topl.utils.codecs.binary.attestation.proposition

import co.topl.attestation.{
  Proposition,
  PublicKeyPropositionCurve25519,
  PublicKeyPropositionEd25519,
  ThresholdPropositionCurve25519
}
import co.topl.utils.codecs.binary.attestation.proposition.PublicKeyPropositionCurve25519Codec.codecs._
import co.topl.utils.codecs.binary.attestation.proposition.PublicKeyPropositionEd25519Codec.codecs._
import co.topl.utils.codecs.binary.attestation.proposition.ThresholdPropositionCurve25519Codec.codecs._
import scodec.Codec
import scodec.codecs.discriminated
import co.topl.utils.codecs.binary.valuetypes.codecs.byte

object PropositionCodec {

  val codec: Codec[Proposition] =
    discriminated[Proposition]
      .by(byte)
      .typecase(PublicKeyPropositionCurve25519.typePrefix, publicKeyPropositionCurve25519)
      .typecase(ThresholdPropositionCurve25519.typePrefix, thresholdPropositionCurve25519)
      .typecase(PublicKeyPropositionEd25519.typePrefix, publicKeyPropositionEd25519)

  trait Codecs {
    val proposition: Codec[Proposition] = codec
  }

  trait Implicits {
    implicit val proposition: Codec[Proposition] = codec
  }

  object codecs extends Codecs

  object implicits extends Implicits
}

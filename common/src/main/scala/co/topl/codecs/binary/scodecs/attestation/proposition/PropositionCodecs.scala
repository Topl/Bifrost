package co.topl.codecs.binary.scodecs.attestation.proposition

import co.topl.attestation.{
  Proposition,
  PublicKeyPropositionCurve25519,
  PublicKeyPropositionEd25519,
  ThresholdPropositionCurve25519
}
import co.topl.crypto.signatures.{Curve25519, Ed25519}
import co.topl.utils.Extensions.LongOps
import scodec.Codec
import scodec.codecs.discriminated
import co.topl.codecs.binary.scodecs.crypto._
import co.topl.codecs.binary.scodecs.valuetypes._
import co.topl.codecs.binary.scodecs.transformers._

import scala.collection.SortedSet

trait PropositionCodecs {

  implicit val publicKeyPropositionCurve25519Codec: Codec[PublicKeyPropositionCurve25519] =
    publicKeyCodec(Curve25519.KeyLength)
      .as[PublicKeyPropositionCurve25519]

  implicit val publicKeyPropositionEd25519Codec: Codec[PublicKeyPropositionEd25519] =
    publicKeyCodec(Ed25519.KeyLength)
      .as[PublicKeyPropositionEd25519]

  implicit val thresholdPropositionCurve25519Codec: Codec[ThresholdPropositionCurve25519] =
    (uIntCodec.xmap[Int](uInt => uInt.toIntExact, int => int) :: listCodec[PublicKeyPropositionCurve25519]
      .as[SortedSet[PublicKeyPropositionCurve25519]])
      .as[ThresholdPropositionCurve25519]

  implicit val propositionCodec: Codec[Proposition] =
    discriminated[Proposition]
      .by(byteCodec)
      .typecase(PublicKeyPropositionCurve25519.typePrefix, publicKeyPropositionCurve25519Codec)
      .typecase(ThresholdPropositionCurve25519.typePrefix, thresholdPropositionCurve25519Codec)
      .typecase(PublicKeyPropositionEd25519.typePrefix, publicKeyPropositionEd25519Codec)
}

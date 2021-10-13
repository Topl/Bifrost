package co.topl.utils.codecs.binary.attestation

import co.topl.attestation._
import co.topl.crypto.signatures.{Curve25519, Ed25519}
import co.topl.utils.codecs.binary.crypto.codecs._
import co.topl.utils.codecs.binary.valuetypes.implicits._
import scodec.Codec
import scodec.codecs.discriminated

import scala.collection.SortedSet

package object proposition {

  trait Codecs {

    implicit val publicKeyPropositionCurve25519Codec: Codec[PublicKeyPropositionCurve25519] =
      publicKeyCodec(Curve25519.KeyLength)
        .as[PublicKeyPropositionCurve25519]

    implicit val publicKeyPropositionEd25519Codec: Codec[PublicKeyPropositionEd25519] =
      publicKeyCodec(Ed25519.KeyLength)
        .as[PublicKeyPropositionEd25519]

    implicit val thresholdPropositionCurve25519Codec: Codec[ThresholdPropositionCurve25519] =
      (intCodec :: listCodec[PublicKeyPropositionCurve25519].as[SortedSet[PublicKeyPropositionCurve25519]])
        .as[ThresholdPropositionCurve25519]

    implicit val propositionCodec: Codec[Proposition] =
      discriminated[Proposition]
        .by(byteCodec)
        .typecase(PublicKeyPropositionCurve25519.typePrefix, publicKeyPropositionCurve25519Codec)
        .typecase(ThresholdPropositionCurve25519.typePrefix, thresholdPropositionCurve25519Codec)
        .typecase(PublicKeyPropositionEd25519.typePrefix, publicKeyPropositionEd25519Codec)
  }

  object codecs extends Codecs
}

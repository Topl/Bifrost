package co.topl.utils.codecs.binary.attestation.proposition

import co.topl.attestation.{PublicKeyPropositionCurve25519, ThresholdPropositionCurve25519}
import co.topl.utils.codecs.binary.attestation.proposition.PublicKeyPropositionCurve25519Codec.implicits._
import scodec.Codec
import co.topl.utils.codecs.binary.valuetypes.codecs._
import co.topl.utils.codecs.binary.valuetypes.implicits._

import scala.collection.SortedSet

object ThresholdPropositionCurve25519Codec {

  val codec: Codec[ThresholdPropositionCurve25519] =
    (int :: list[PublicKeyPropositionCurve25519].as[SortedSet[PublicKeyPropositionCurve25519]])
      .as[ThresholdPropositionCurve25519]

  trait Codecs {
    val thresholdPropositionCurve25519: Codec[ThresholdPropositionCurve25519] = codec
  }

  trait Implicits {
    implicit val implicitThresholdPropositionCurve25519: Codec[ThresholdPropositionCurve25519] = codec
  }

  object codecs extends Codecs

  object implicits extends Implicits
}

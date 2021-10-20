package co.topl.utils.codecs.binary.attestation.proof

import cats.{Eq, Show}
import co.topl.attestation.{SignatureEd25519, ThresholdSignatureCurve25519}
import co.topl.utils.CommonGenerators
import co.topl.utils.codecs.binary.CodecCompatabilityBehavior
import co.topl.utils.codecs.binary.attestation.proof.codecs._
import co.topl.utils.codecs.binary.legacy.attestation.ThresholdSignatureCurve25519Serializer

class ThresholdSignatureCurve25519CodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  implicit private val eq: Eq[ThresholdSignatureCurve25519] = Eq.fromUniversalEquals
  implicit private val show: Show[ThresholdSignatureCurve25519] = Show.fromToString

  codecCompatabilityBehavior(
    "threshold signature curve 25519",
    thresholdSignatureCurve25519Codec,
    ThresholdSignatureCurve25519Serializer,
    thresholdSignatureCurve25519Gen
  )
}

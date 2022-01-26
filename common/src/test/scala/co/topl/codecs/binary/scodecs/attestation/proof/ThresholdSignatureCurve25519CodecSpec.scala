package co.topl.codecs.binary.scodecs.attestation.proof

import co.topl.codecs.binary.CodecCompatabilityBehavior
import co.topl.codecs.binary.legacy.attestation.ThresholdSignatureCurve25519Serializer
import co.topl.utils.CommonGenerators
import co.topl.utils.catsInstances._

class ThresholdSignatureCurve25519CodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  codecCompatabilityBehavior(
    "threshold signature curve 25519",
    thresholdSignatureCurve25519Codec,
    ThresholdSignatureCurve25519Serializer,
    thresholdSignatureCurve25519Gen
  )
}

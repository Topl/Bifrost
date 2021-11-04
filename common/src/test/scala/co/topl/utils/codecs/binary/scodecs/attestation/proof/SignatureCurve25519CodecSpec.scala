package co.topl.utils.codecs.binary.scodecs.attestation.proof

import cats.{Eq, Show}
import co.topl.attestation.SignatureCurve25519
import co.topl.utils.CommonGenerators
import co.topl.utils.codecs.binary.CodecCompatabilityBehavior
import co.topl.utils.codecs.binary.legacy.attestation.SignatureCurve25519Serializer

class SignatureCurve25519CodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  implicit private val eq: Eq[SignatureCurve25519] = Eq.fromUniversalEquals
  implicit private val show: Show[SignatureCurve25519] = Show.fromToString

  codecCompatabilityBehavior(
    "signature curve 25519",
    signatureCurve25519Codec,
    SignatureCurve25519Serializer,
    signatureCurve25519Gen
  )
}

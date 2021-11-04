package co.topl.utils.codecs.binary.scodecs.attestation.proof

import cats.{Eq, Show}
import co.topl.attestation.Proof
import co.topl.utils.CommonGenerators
import co.topl.utils.codecs.binary.CodecCompatabilityBehavior
import co.topl.utils.codecs.binary.legacy.attestation.ProofSerializer
import org.scalacheck.Gen

class ProofCodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  implicit private val eq: Eq[Proof[_]] = Eq.fromUniversalEquals
  implicit private val show: Show[Proof[_]] = Show.fromToString

  codecCompatabilityBehavior(
    "proof",
    proofCodec,
    ProofSerializer,
    Gen.oneOf(signatureCurve25519Gen, signatureEd25519Gen, thresholdSignatureCurve25519Gen)
  )
}

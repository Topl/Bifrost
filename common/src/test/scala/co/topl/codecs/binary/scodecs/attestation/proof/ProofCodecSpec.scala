package co.topl.codecs.binary.scodecs.attestation.proof

import co.topl.codecs.binary.CodecCompatabilityBehavior
import co.topl.codecs.binary.legacy.attestation.ProofSerializer
import co.topl.utils.CommonGenerators
import co.topl.utils.implicits._
import org.scalacheck.Gen

class ProofCodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  codecCompatabilityBehavior(
    "signature curve 25519 proof",
    proofCodec,
    ProofSerializer,
    Gen.oneOf(signatureCurve25519Gen, signatureEd25519Gen, thresholdSignatureCurve25519Gen)
  )
}

package co.topl.codecs.binary.scodecs.attestation.proof

import co.topl.codecs.binary.CodecCompatabilityBehavior
import co.topl.codecs.binary.legacy.attestation.SignatureCurve25519Serializer
import co.topl.utils.CommonGenerators
import co.topl.utils.implicits._

class SignatureCurve25519CodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  codecCompatabilityBehavior(
    "signature curve 25519",
    signatureCurve25519Codec,
    SignatureCurve25519Serializer,
    signatureCurve25519Gen
  )
}

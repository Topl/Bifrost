package co.topl.codecs.binary.scodecs.attestation.proof

import co.topl.codecs.binary.CodecCompatabilityBehavior
import co.topl.codecs.binary.legacy.attestation.SignatureEd25519Serializer
import co.topl.utils.CommonGenerators
import co.topl.utils.implicits._

class SignatureEd25519CodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  codecCompatabilityBehavior(
    "signature ed 25519",
    signatureEd25519Codec,
    SignatureEd25519Serializer,
    signatureEd25519Gen
  )
}

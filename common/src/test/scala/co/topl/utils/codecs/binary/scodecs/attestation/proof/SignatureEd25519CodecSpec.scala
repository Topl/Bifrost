package co.topl.utils.codecs.binary.scodecs.attestation.proof

import cats.{Eq, Show}
import co.topl.attestation.SignatureEd25519
import co.topl.utils.CommonGenerators
import co.topl.utils.codecs.binary.CodecCompatabilityBehavior
import co.topl.utils.codecs.binary.legacy.attestation.SignatureEd25519Serializer

class SignatureEd25519CodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  implicit private val eq: Eq[SignatureEd25519] = Eq.fromUniversalEquals
  implicit private val show: Show[SignatureEd25519] = Show.fromToString

  codecCompatabilityBehavior(
    "signature ed 25519",
    signatureEd25519Codec,
    SignatureEd25519Serializer,
    signatureEd25519Gen
  )
}

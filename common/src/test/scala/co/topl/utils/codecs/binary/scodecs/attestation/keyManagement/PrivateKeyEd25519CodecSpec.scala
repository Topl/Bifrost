package co.topl.utils.codecs.binary.scodecs.attestation.keyManagement

import cats.{Eq, Show}
import co.topl.attestation.keyManagement.PrivateKeyEd25519
import co.topl.utils.CommonGenerators
import co.topl.utils.codecs.binary.CodecCompatabilityBehavior
import co.topl.utils.codecs.binary.legacy.attestation.keyManagement.PrivateKeyEd25519Serializer

class PrivateKeyEd25519CodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  implicit private val show: Show[PrivateKeyEd25519] = Show.fromToString

  implicit private val eq: Eq[PrivateKeyEd25519] = Eq.fromUniversalEquals

  codecCompatabilityBehavior(
    "private key ed 25519",
    privateKeyEd25519Codec,
    PrivateKeyEd25519Serializer,
    keyEd25519Gen.map(_._1)
  )
}

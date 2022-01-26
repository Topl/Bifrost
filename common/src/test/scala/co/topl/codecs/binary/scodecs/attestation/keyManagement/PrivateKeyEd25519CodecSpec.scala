package co.topl.codecs.binary.scodecs.attestation.keyManagement

import co.topl.codecs.binary.CodecCompatabilityBehavior
import co.topl.codecs.binary.legacy.attestation.keyManagement.PrivateKeyEd25519Serializer
import co.topl.utils.CommonGenerators
import co.topl.utils.catsInstances._

class PrivateKeyEd25519CodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  codecCompatabilityBehavior(
    "private key ed 25519",
    privateKeyEd25519Codec,
    PrivateKeyEd25519Serializer,
    keyEd25519Gen.map(_._1)
  )
}

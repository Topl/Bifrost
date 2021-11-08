package co.topl.codecs.binary.scodecs.attestation.keyManagement

import co.topl.codecs.binary.CodecCompatabilityBehavior
import co.topl.codecs.binary.legacy.attestation.keyManagement.PrivateKeyCurve25519Serializer
import co.topl.utils.CommonGenerators
import co.topl.utils.catsInstances._

class PrivateKeyCurve25519CodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  codecCompatabilityBehavior(
    "private key curve 25519",
    privateKeyCurve25519Codec,
    PrivateKeyCurve25519Serializer,
    keyCurve25519Gen.map(_._1)
  )
}

package co.topl.utils.codecs.binary.attestation.keyManagement

import cats.{Eq, Show}
import co.topl.attestation.keyManagement.PrivateKeyCurve25519
import co.topl.utils.CommonGenerators
import co.topl.utils.codecs.binary.attestation.keyManagement.codecs._
import co.topl.utils.codecs.binary.CodecCompatabilityBehavior
import co.topl.utils.codecs.binary.legacy.attestation.keyManagement.PrivateKeyCurve25519Serializer

class PrivateKeyCurve25519CodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  implicit private val show: Show[PrivateKeyCurve25519] = Show.fromToString

  implicit private val eq: Eq[PrivateKeyCurve25519] = Eq.fromUniversalEquals

  codecCompatabilityBehavior(
    "private key curve 25519",
    privateKeyCurve25519Codec,
    PrivateKeyCurve25519Serializer,
    keyCurve25519Gen.map(_._1)
  )
}

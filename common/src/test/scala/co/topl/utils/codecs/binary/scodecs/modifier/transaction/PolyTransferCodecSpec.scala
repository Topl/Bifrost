package co.topl.utils.codecs.binary.scodecs.modifier.transaction

import cats.{Eq, Show}
import co.topl.attestation.Proposition
import co.topl.modifier.transaction.PolyTransfer
import co.topl.utils.CommonGenerators
import co.topl.utils.codecs.binary.CodecCompatabilityBehavior
import co.topl.utils.codecs.binary.legacy.modifier.transaction.PolyTransferSerializer

class PolyTransferCodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  implicit private val eq: Eq[PolyTransfer[_ <: Proposition]] = Eq.fromUniversalEquals
  implicit private val show: Show[PolyTransfer[_ <: Proposition]] = Show.fromToString

  codecCompatabilityBehavior("poly transfer", polyTransferCodec, PolyTransferSerializer, polyTransferGen)
}

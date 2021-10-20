package co.topl.utils.codecs.binary.modifier.transaction

import cats.{Eq, Show}
import co.topl.attestation.Proposition
import co.topl.modifier.transaction.ArbitTransfer
import co.topl.utils.CommonGenerators
import co.topl.utils.codecs.binary.CodecCompatabilityBehavior
import co.topl.utils.codecs.binary.legacy.modifier.transaction.ArbitTransferSerializer
import co.topl.utils.codecs.binary.modifier.codecs.arbitTransferCodec

class ArbitTransferCodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  implicit private val eq: Eq[ArbitTransfer[_ <: Proposition]] = Eq.fromUniversalEquals
  implicit private val show: Show[ArbitTransfer[_ <: Proposition]] = Show.fromToString

  codecCompatabilityBehavior("arbit transfer", arbitTransferCodec, ArbitTransferSerializer, arbitTransferGen)
}

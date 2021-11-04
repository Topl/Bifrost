package co.topl.utils.codecs.binary.scodecs.attestation.proposition

import cats.{Eq, Show}
import co.topl.attestation.Proposition
import co.topl.utils.CommonGenerators
import co.topl.utils.codecs.binary.CodecCompatabilityBehavior
import co.topl.utils.codecs.binary.legacy.attestation.PropositionSerializer

class PropositionCodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  implicit private val eq: Eq[Proposition] = Eq.fromUniversalEquals
  implicit private val show: Show[Proposition] = Show.fromToString

  codecCompatabilityBehavior("proposition", propositionCodec, PropositionSerializer, propositionGen)
}

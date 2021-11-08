package co.topl.codecs.binary.scodecs.attestation.proposition

import co.topl.codecs.binary.CodecCompatabilityBehavior
import co.topl.codecs.binary.legacy.attestation.PropositionSerializer
import co.topl.utils.CommonGenerators
import co.topl.utils.catsInstances._

class PropositionCodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  codecCompatabilityBehavior("proposition", propositionCodec, PropositionSerializer, propositionGen)
}

package co.topl.attestation

import co.topl.attestation.Evidence.EvidenceTypePrefix
import co.topl.utils.serialization.BytesSerializable

// Propositions are challenges that must be satisfied by the prover.
// In most cases, propositions are used by transactions issuers (spenders) to prove the right
// to use a UTXO in a transaction.
sealed trait Proposition extends BytesSerializable {
  val typePrefix: EvidenceTypePrefix
}

// Knowledge propositions require the prover to supply a proof attesting to their knowledge
// of secret information.
trait KnowledgeProposition[S <: Secret] extends Proposition


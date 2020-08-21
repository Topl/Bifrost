package crypto

import serialization.BytesSerializable

trait Proposition extends BytesSerializable

trait ProofOfKnowledgeProposition[S <: Secret] extends Proposition


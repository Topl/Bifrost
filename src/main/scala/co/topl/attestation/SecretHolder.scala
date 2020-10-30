package co.topl.attestation

import co.topl.attestation.proof.ProofOfKnowledge
import co.topl.attestation.proposition.KnowledgeProposition
import co.topl.utils.serialization.BytesSerializable

trait Secret extends BytesSerializable {
  self =>

  type M = S
  type S >: self.type <: Secret
  type PK <: KnowledgeProposition[S]
  type PR <: ProofOfKnowledge[S, _ <: KnowledgeProposition[S]]

  def companion: SecretCompanion[S]

  def instance: S = self

  def publicImage: PK

  def sign(message: Array[Byte]): PR
}

trait SecretCompanion[S <: Secret] {
  type PK = S#PK
  type PR = S#PR

  def verify(message: Array[Byte], publicImage: PK, proof: PR): Boolean

  def generateKeys(randomSeed: Array[Byte]): (S, PK)
}

package co.topl.crypto

import co.topl.nodeView.state.box._
import co.topl.nodeView.state.box.proposition.ProofOfKnowledgeProposition
import co.topl.utils.serialization.BytesSerializable

trait Secret extends BytesSerializable {
  self =>
  type S >: self.type <: Secret
  type PK <: ProofOfKnowledgeProposition[S]

  def companion: SecretCompanion[S]

  def instance: S = self

  def publicImage: PK
}

trait SecretCompanion[S <: Secret] {
  type PK = S#PK

  type PR <: ProofOfKnowledge[S, _ <: ProofOfKnowledgeProposition[S]]

  def owns(secret: S, box: GenericBox[_, Long]): Boolean

  def sign(secret: S, message: Array[Byte]): PR

  def verify(message: Array[Byte], publicImage: PK, proof: PR): Boolean

  def generateKeys(randomSeed: Array[Byte]): (S, PK)
}

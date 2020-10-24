package co.topl.crypto

import co.topl.nodeView.state.box._
import co.topl.utils.serialization.BytesSerializable

trait Secret extends BytesSerializable {
  self =>

  type M = S
  type S >: self.type <: Secret
  type PK <: ProofOfKnowledgeProposition[S]
  type PR <: ProofOfKnowledge[S, _ <: ProofOfKnowledgeProposition[S]]

  def companion: SecretCompanion[S]

  def instance: S = self

  def publicImage: PK

  def owns(box: GenericBox[_ <: PK, _]): Boolean

  def sign(message: Array[Byte]): PR
}

trait SecretCompanion[S <: Secret] {
  type PK = S#PK
  type PR = S#PR

  def verify(message: Array[Byte], publicImage: PK, proof: PR): Boolean

  def generateKeys(randomSeed: Array[Byte]): (S, PK)
}

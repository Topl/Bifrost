package crypto

import keymanager.Keyfile
import utils.serialization.BytesSerializable

trait Secret extends BytesSerializable {
  self =>

  type M = S
  type S >: self.type <: Secret
  type PK <: KnowledgeProposition[S]
  type PR <: ProofOfKnowledge[S, _ <: KnowledgeProposition[S]]
  type KF <: Keyfile[S]

  def instance: S = self

  def publicImage: PK

  def sign(message: Array[Byte]): PR
}

package attestation

/**
 * A Secret is equivalent to the typical private key
 * Right now we only have one type of secret [[PrivateKeyCurve25519]]
 */
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

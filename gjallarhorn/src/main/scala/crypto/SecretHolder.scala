package crypto

import scorex.crypto.signatures.{Curve25519, PrivateKey, PublicKey}
import serialization.BytesSerializable
import utils.serialization.{GjalSerializer, Reader, Writer}

trait Secret extends BytesSerializable {
  self =>

  type M = S
  type S >: self.type <: Secret
  type PK <: ProofOfKnowledgeProposition[S]
  type PR <: ProofOfKnowledge[S, _ <: ProofOfKnowledgeProposition[S]]

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


case class PrivateKey25519(privKeyBytes: PrivateKey,
                           publicKeyBytes: PublicKey
                          ) extends Secret {

  require(privKeyBytes.length == Curve25519.KeyLength, s"${privKeyBytes.length} == ${Curve25519.KeyLength}")
  require(publicKeyBytes.length == Curve25519.KeyLength, s"${publicKeyBytes.length} == ${Curve25519.KeyLength}")

  override type S = PrivateKey25519
  override type PK = PublicKey25519Proposition
  override type PR = Signature25519

  override lazy val companion: SecretCompanion[PrivateKey25519] = PrivateKey25519

  override lazy val serializer: GjalSerializer[PrivateKey25519] = PrivateKey25519Serializer

  override lazy val publicImage: PublicKey25519Proposition = PublicKey25519Proposition(publicKeyBytes)

  override def sign(message: Array[Byte]): Signature25519 = Signature25519(Curve25519.sign(this.privKeyBytes, message))

  override def equals(o: Any): Boolean = {
    o.isInstanceOf[PrivateKey25519] &&
      java.util.Arrays.equals(privKeyBytes, o.asInstanceOf[PrivateKey25519].privKeyBytes)
  }

}

object PrivateKey25519 extends SecretCompanion[PrivateKey25519] {

  override def verify(message: Array[Byte], publicImage: PublicKey25519Proposition, proof: Signature25519): Boolean =
    Curve25519.verify(proof.signature, message, publicImage.pubKeyBytes)

  override def generateKeys(randomSeed: Array[Byte]): (PrivateKey25519, PublicKey25519Proposition) = {
    val (sk, pk) = Curve25519.createKeyPair(randomSeed)
    val secret: PrivateKey25519 = new PrivateKey25519(sk, pk)
    secret -> secret.publicImage
  }
}

object PrivateKey25519Serializer extends GjalSerializer[PrivateKey25519] {
  override def serialize(obj: PrivateKey25519, w: Writer): Unit = {
    /* privKeyBytes: Array[Byte] */
    w.putBytes(obj.privKeyBytes)

    /* publicKeyBytes: Array[Byte] */
    w.putBytes(obj.publicKeyBytes)
  }

  override def parse(r: Reader): PrivateKey25519 = {
    PrivateKey25519(PrivateKey @@ r.getBytes(Curve25519.KeyLength), PublicKey @@ r.getBytes(Curve25519.KeyLength))
  }
}
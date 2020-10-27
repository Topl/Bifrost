package co.topl.attestation.secrets

import co.topl.attestation.proof.Signature25519
import co.topl.attestation.proposition.PublicKey25519Proposition
import co.topl.attestation.secrets.serialization.PrivateKey25519Serializer
import co.topl.utils.serialization.BifrostSerializer
import scorex.crypto.signatures.{ Curve25519, PrivateKey, PublicKey }

case class PrivateKey25519( private[crypto] val privKeyBytes: PrivateKey,
                            private[crypto] val publicKeyBytes: PublicKey
                          ) extends Secret {

  require(privKeyBytes.length == Curve25519.KeyLength, s"${privKeyBytes.length} == ${Curve25519.KeyLength}")
  require(publicKeyBytes.length == Curve25519.KeyLength, s"${publicKeyBytes.length} == ${Curve25519.KeyLength}")

  override type S = PrivateKey25519
  override type PK = PublicKey25519Proposition
  override type PR = Signature25519

  override lazy val companion: SecretCompanion[PrivateKey25519] = PrivateKey25519

  override lazy val serializer: BifrostSerializer[PrivateKey25519] = PrivateKey25519Serializer

  override lazy val publicImage: PublicKey25519Proposition = PublicKey25519Proposition(publicKeyBytes)

  //override def owns(box: GenericBox[_ <: PK, _]): Boolean = box.proposition == this.publicImage

  override def sign(message: Array[Byte]): Signature25519 = Signature25519(Curve25519.sign(this.privKeyBytes, message))

  override def equals(obj: Any): Boolean = obj match {
    case sk: PrivateKey25519 => sk.privKeyBytes sameElements privKeyBytes
    case _ => false
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

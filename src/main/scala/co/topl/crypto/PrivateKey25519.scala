package co.topl.crypto

import co.topl.crypto.serialization.PrivateKey25519Serializer
import co.topl.nodeView.state.box.GenericBox
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import co.topl.utils.serialization.BifrostSerializer
import scorex.crypto.signatures.Curve25519

class PrivateKey25519(privKeyBytes: Array[Byte], publicKeyBytes: Array[Byte] ) extends Secret {

  require(privKeyBytes.length == Curve25519.KeyLength, s"${privKeyBytes.length} == ${Curve25519.KeyLength}")
  require(publicKeyBytes.length == Curve25519.KeyLength, s"${publicKeyBytes.length} == ${Curve25519.KeyLength}")

  override type S = PrivateKey25519
  override type PK = PublicKey25519Proposition
  override type PR = Signature25519

  override lazy val companion: SecretCompanion[PrivateKey25519] = PrivateKey25519

  override lazy val publicImage: PublicKey25519Proposition = PublicKey25519Proposition(publicKeyBytes)

  override def owns(box: GenericBox[_ <: PK, _]): Boolean = box.proposition == this.publicImage

  override def sign(message: Array[Byte]): Signature25519 = Signature25519(Curve25519.sign(this.privKeyBytes, message))

  override def serializer: BifrostSerializer[PrivateKey25519] = PrivateKey25519Serializer

  override def equals(o: Any): Boolean = {
    o.isInstanceOf[PrivateKey25519] &&
      java.util.Arrays.equals(privKeyBytes, o.asInstanceOf[PrivateKey25519].privKeyBytes)
  }
}



object PrivateKey25519 extends SecretCompanion[PrivateKey25519] {

  override def verify(message: Array[Byte], publicImage: PublicKey25519Proposition, proof: Signature25519): Boolean =
    Curve25519.verify(proof.signature, message, publicImage.bytes)

  override def generateKeys(randomSeed: Array[Byte]): (PrivateKey25519, PublicKey25519Proposition) = {
    val (sk, pk) = Curve25519.createKeyPair(randomSeed)
    val secret: PrivateKey25519 = new PrivateKey25519(sk, pk)
    secret -> secret.publicImage
  }
}

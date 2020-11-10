package co.topl.attestation.secrets

import co.topl.attestation.proof.SignatureCurve25519
import co.topl.attestation.proposition.PublicKeyPropositionCurve25519
import co.topl.utils.serialization.BifrostSerializer
import scorex.crypto.signatures.{ Curve25519, PrivateKey, PublicKey, Signature }

case class PrivateKeyCurve25519 (private[crypto] val privKeyBytes  : PrivateKey,
                                 private[crypto] val publicKeyBytes: PublicKey
                          ) extends Secret {

  require(privKeyBytes.length == Curve25519.KeyLength, s"${privKeyBytes.length} == ${Curve25519.KeyLength}")
  require(publicKeyBytes.length == Curve25519.KeyLength, s"${publicKeyBytes.length} == ${Curve25519.KeyLength}")

  override type S = PrivateKeyCurve25519
  override type PK = PublicKeyPropositionCurve25519
  override type PR = SignatureCurve25519

  override lazy val companion: SecretCompanion[PrivateKeyCurve25519] = PrivateKeyCurve25519

  override lazy val serializer: BifrostSerializer[PrivateKeyCurve25519] = PrivateKeyCurve25519Serializer

  override lazy val publicImage: PublicKeyPropositionCurve25519 = PublicKeyPropositionCurve25519(publicKeyBytes)

  override def sign ( message: Array[Byte] ): SignatureCurve25519 = SignatureCurve25519(Curve25519.sign(this.privKeyBytes, message))

  override def equals ( obj: Any ): Boolean = obj match {
    case sk: PrivateKeyCurve25519 => sk.privKeyBytes sameElements privKeyBytes
    case _                        => false
  }
}



object PrivateKeyCurve25519 extends SecretCompanion[PrivateKeyCurve25519] {

  implicit val secretGenerator: SecretGenerator[PrivateKeyCurve25519] =
    SecretGenerator.instance[PrivateKeyCurve25519] {
      seed: Array[Byte] => generateKeys(seed)._1
    }

  override def verify( message: Array[Byte], publicImage: PublicKeyPropositionCurve25519, proof: SignatureCurve25519): Boolean =
    Curve25519.verify(Signature @@ proof.bytes, message, PublicKey @@ publicImage.bytes)

  override def generateKeys(randomSeed: Array[Byte]): (PrivateKeyCurve25519, PublicKeyPropositionCurve25519) = {
    val (sk, pk) = Curve25519.createKeyPair(randomSeed)
    val secret: PrivateKeyCurve25519 = PrivateKeyCurve25519(sk, pk)
    secret -> secret.publicImage
  }
}

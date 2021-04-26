package attestation

import crypto.KeyfileCurve25519
import co.topl.crypto.signatures.{Curve25519, PrivateKey, PublicKey}
import utils.serialization.{GjalSerializer, Reader, Writer}

/**
  * A Secret corresponding to a PublicKeyCurve25519 proposition.
  * @param privKeyBytes - array of bytes that form the private key
  * @param publicKeyBytes - array of bytes that form the public key
  */
case class PrivateKeyCurve25519 (private val privKeyBytes  : PrivateKey,
                                 private val publicKeyBytes: PublicKey
                                ) extends Secret {

  require(
    privKeyBytes.toBytes.length == Curve25519.KeyLength,
    s"${privKeyBytes.toBytes.length} == ${Curve25519.KeyLength}")
  require(
    publicKeyBytes.toBytes.length == Curve25519.KeyLength,
    s"${publicKeyBytes.toBytes.length} == ${Curve25519.KeyLength}")

  override type S = PrivateKeyCurve25519
  override type PK = PublicKeyPropositionCurve25519
  override type PR = SignatureCurve25519
  override type KF = KeyfileCurve25519

  override lazy val serializer: GjalSerializer[PrivateKeyCurve25519] = PrivateKeyCurve25519

  override lazy val publicImage: PublicKeyPropositionCurve25519 = PublicKeyPropositionCurve25519(publicKeyBytes)

  override def sign (message: Array[Byte]): SignatureCurve25519 =
    SignatureCurve25519(Curve25519.sign(privKeyBytes, message))

  override def equals ( obj: Any ): Boolean = obj match {
    case sk: PrivateKeyCurve25519 => sk.privKeyBytes.toBytes sameElements privKeyBytes.toBytes
    case _                        => false
  }
}



object PrivateKeyCurve25519 extends GjalSerializer[PrivateKeyCurve25519] {

  implicit val secretGenerator: SecretGenerator[PrivateKeyCurve25519] =
    SecretGenerator.instance[PrivateKeyCurve25519] {
      seed: Array[Byte] => {
        val (sk, pk) = Curve25519.createKeyPair(seed)
        val secret: PrivateKeyCurve25519 = PrivateKeyCurve25519(sk, pk)
        secret -> secret.publicImage
      }
    }

  override def serialize(obj: PrivateKeyCurve25519, w: Writer): Unit = {
    /* privKeyBytes: Array[Byte] */
    w.putBytes(obj.privKeyBytes.toBytes)

    /* publicKeyBytes: Array[Byte] */
    w.putBytes(obj.publicKeyBytes.toBytes)
  }

  override def parse(r: Reader): PrivateKeyCurve25519 = {
    PrivateKeyCurve25519(PrivateKey(r.getBytes(Curve25519.KeyLength)), PublicKey(r.getBytes(Curve25519.KeyLength)))
  }

}
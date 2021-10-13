package co.topl.attestation.keyManagement

import cats.implicits._
import co.topl.attestation.{PublicKeyPropositionCurve25519, SignatureCurve25519}
import co.topl.crypto.implicits._
import co.topl.crypto.signing.{Curve25519, MessageToSign, Seed}
import co.topl.crypto.{PrivateKey, PublicKey}
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Sized
import co.topl.models.{Bytes, SecretKeys}
import co.topl.utils.serialization.{BifrostSerializer, Reader, Writer}

class PrivateKeyCurve25519(private val privateKey: PrivateKey, private val publicKey: PublicKey) extends Secret {

  private val curve25519 = new Curve25519()
  private val privateKeyLength = privateKey.value.length
  private val publicKeyLength = publicKey.value.length

  require(privateKeyLength == curve25519.KeyLength, s"$privateKeyLength == ${curve25519.KeyLength}")
  require(publicKeyLength == curve25519.KeyLength, s"$publicKeyLength == ${curve25519.KeyLength}")

  override type S = PrivateKeyCurve25519
  override type PK = PublicKeyPropositionCurve25519
  override type PR = SignatureCurve25519
  override type KF = KeyfileCurve25519

  override lazy val serializer: BifrostSerializer[PrivateKeyCurve25519] = PrivateKeyCurve25519

  override lazy val publicImage: PublicKeyPropositionCurve25519 = PublicKeyPropositionCurve25519(publicKey)

  override def sign(message: Array[Byte]): SignatureCurve25519 = SignatureCurve25519(
    curve25519.sign(SecretKeys.Curve25519(Sized.strictUnsafe(Bytes(privateKey.value))), MessageToSign(message))
  )

  override def equals(obj: Any): Boolean = obj match {
    case sk: PrivateKeyCurve25519 => sk.privateKey === privateKey
    case _                        => false
  }
}

object PrivateKeyCurve25519 extends BifrostSerializer[PrivateKeyCurve25519] {

  implicit val secretGenerator: SecretGenerator[PrivateKeyCurve25519] =
    SecretGenerator.instance[PrivateKeyCurve25519] { seed: Array[Byte] =>
      val curve25519 = new Curve25519
      val (sk, pk) = curve25519.createKeyPair(Seed(seed))
      val secret: PrivateKeyCurve25519 =
        new PrivateKeyCurve25519(PrivateKey(sk.bytes.data.toArray), PublicKey(pk.bytes.data.toArray))
      secret -> secret.publicImage
    }

  override def serialize(obj: PrivateKeyCurve25519, w: Writer): Unit = {
    /* : Array[Byte] */
    w.putBytes(obj.privateKey.value)

    /* publicKeyBytes: Array[Byte] */
    w.putBytes(obj.publicKey.value)
  }

  override def parse(r: Reader): PrivateKeyCurve25519 =
    new PrivateKeyCurve25519(
      PrivateKey(r.getBytes(Curve25519.instance.KeyLength)),
      PublicKey(r.getBytes(Curve25519.instance.KeyLength))
    )

}

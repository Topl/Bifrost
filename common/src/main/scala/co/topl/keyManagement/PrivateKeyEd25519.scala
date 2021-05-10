package co.topl.keyManagement

import cats.implicits._
import co.topl.attestation.{PublicKeyPropositionEd25519, SignatureEd25519}
import co.topl.crypto.signatures.eddsa.Ed25519
import co.topl.crypto.signatures.{PrivateKey, PublicKey}
import co.topl.utils.BytesOf
import co.topl.utils.BytesOf.Implicits._
import co.topl.utils.serialization.{BifrostSerializer, Reader, Writer}

case class PrivateKeyEd25519(private val privKeyBytes: PrivateKey, private val publicKeyBytes: PublicKey)
    extends Secret {

  private val privateKeyLength = BytesOf[PrivateKey].length(privKeyBytes)
  private val publicKeyLength = BytesOf[PublicKey].length(publicKeyBytes)

  require(privateKeyLength == Ed25519.KeyLength, s"$privateKeyLength == ${Ed25519.KeyLength}")
  require(publicKeyLength == Ed25519.KeyLength, s"$publicKeyLength == ${Ed25519.KeyLength}")

  override type S = PrivateKeyEd25519
  override type PK = PublicKeyPropositionEd25519
  override type PR = SignatureEd25519
  override type KF = KeyfileEd25519

  override lazy val serializer: BifrostSerializer[PrivateKeyEd25519] = PrivateKeyEd25519

  override lazy val publicImage: PublicKeyPropositionEd25519 = PublicKeyPropositionEd25519(publicKeyBytes)

  override def sign(message: Array[Byte]): SignatureEd25519 = SignatureEd25519(
    Ed25519.sign(privKeyBytes, message)
  )

  override def equals(obj: Any): Boolean = obj match {
    case sk: PrivateKeyEd25519 => sk.privKeyBytes === privKeyBytes
    case _                     => false
  }
}

object PrivateKeyEd25519 extends BifrostSerializer[PrivateKeyEd25519] {

  implicit val secretGenerator: SecretGenerator[PrivateKeyEd25519] =
    SecretGenerator.instance[PrivateKeyEd25519] { seed: Array[Byte] =>
      val (sk, pk) = Ed25519.createKeyPair(seed)
      val secret: PrivateKeyEd25519 = PrivateKeyEd25519(sk, pk)
      secret -> secret.publicImage
    }

  override def serialize(obj: PrivateKeyEd25519, w: Writer): Unit = {
    /* privKeyBytes: Array[Byte] */
    w.putBytes(obj.privKeyBytes)

    /* publicKeyBytes: Array[Byte] */
    w.putBytes(obj.publicKeyBytes)
  }

  override def parse(r: Reader): PrivateKeyEd25519 =
    PrivateKeyEd25519(PrivateKey(r.getBytes(Ed25519.KeyLength)), PublicKey(r.getBytes(Ed25519.KeyLength)))

}

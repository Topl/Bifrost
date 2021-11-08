package co.topl.attestation.keyManagement

import cats.implicits._
import co.topl.attestation.{PublicKeyPropositionEd25519, SignatureEd25519}
import co.topl.crypto.implicits._
import co.topl.crypto.signatures.Ed25519
import co.topl.crypto.{PrivateKey, PublicKey}
import co.topl.codecs.binary.legacy.BifrostSerializer
import co.topl.codecs.binary.legacy.attestation.keyManagement.PrivateKeyEd25519Serializer

case class PrivateKeyEd25519(privateKey: PrivateKey, publicKey: PublicKey) extends Secret {

  private val privateKeyLength = privateKey.value.length
  private val publicKeyLength = publicKey.value.length
  private val ec = new Ed25519

  require(privateKeyLength == ec.KeyLength, s"$privateKeyLength == ${ec.KeyLength}")
  require(publicKeyLength == ec.KeyLength, s"$publicKeyLength == ${ec.KeyLength}")

  override type S = PrivateKeyEd25519
  override type PK = PublicKeyPropositionEd25519
  override type PR = SignatureEd25519
  override type KF = KeyfileEd25519

  override lazy val serializer: BifrostSerializer[PrivateKeyEd25519] = PrivateKeyEd25519Serializer

  override lazy val publicImage: PublicKeyPropositionEd25519 = PublicKeyPropositionEd25519(publicKey)

  override def sign(message: Array[Byte]): SignatureEd25519 = SignatureEd25519(
    ec.sign(privateKey, message)
  )

  override def equals(obj: Any): Boolean = obj match {
    case sk: PrivateKeyEd25519 => sk.privateKey === privateKey
    case _                     => false
  }
}

object PrivateKeyEd25519 {

  implicit val secretGenerator: SecretGenerator[PrivateKeyEd25519] =
    SecretGenerator.instance[PrivateKeyEd25519] { seed: Array[Byte] =>
      val ec = new Ed25519
      val (sk, pk) = ec.createKeyPair(seed)
      val secret: PrivateKeyEd25519 = PrivateKeyEd25519(sk, pk)
      secret -> secret.publicImage
    }

}

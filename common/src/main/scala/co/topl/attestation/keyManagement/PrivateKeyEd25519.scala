package co.topl.attestation.keyManagement

import cats.implicits._
import co.topl.attestation.{PublicKeyPropositionEd25519, SignatureEd25519}
import co.topl.codecs.binary.legacy.BifrostSerializer
import co.topl.codecs.binary.legacy.attestation.keyManagement.PrivateKeyEd25519Serializer
import co.topl.crypto.implicits._
import co.topl.crypto.generation.mnemonic.Entropy
import co.topl.crypto.signing.Ed25519
import co.topl.crypto.{PrivateKey, PublicKey, Signature}
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Sized
import co.topl.models.{Bytes, SecretKeys}

//noinspection ScalaStyle
case class PrivateKeyEd25519(privateKey: PrivateKey, publicKey: PublicKey) extends Secret {

  private val ed25519 = new Ed25519()
  private val privateKeyLength = privateKey.value.length
  private val publicKeyLength = publicKey.value.length

  require(privateKeyLength == ed25519.KeyLength, s"$privateKeyLength == ${ed25519.KeyLength}")
  require(publicKeyLength == ed25519.KeyLength, s"$publicKeyLength == ${ed25519.KeyLength}")

  override type S = PrivateKeyEd25519
  override type PK = PublicKeyPropositionEd25519
  override type PR = SignatureEd25519
  override type KF = KeyfileEd25519

  override lazy val serializer: BifrostSerializer[PrivateKeyEd25519] = PrivateKeyEd25519Serializer

  override lazy val publicImage: PublicKeyPropositionEd25519 = PublicKeyPropositionEd25519(publicKey)

  override def sign(message: Array[Byte]): SignatureEd25519 = SignatureEd25519(
    Signature(
      ed25519
        .sign(SecretKeys.Ed25519(Sized.strictUnsafe(Bytes(privateKey.value))), Bytes(message))
        .bytes
        .data
        .toArray
    )
  )

  override def equals(obj: Any): Boolean = obj match {
    case sk: PrivateKeyEd25519 => sk.privateKey === privateKey
    case _                     => false
  }
}

object PrivateKeyEd25519 {

  implicit val secretGenerator: SecretGenerator[PrivateKeyEd25519] =
    SecretGenerator.instance[PrivateKeyEd25519] { seed: Array[Byte] =>
      val (sk, pk) = Ed25519.instance.deriveKeyPairFromEntropy(Entropy(Bytes(seed)), None)
      val secret: PrivateKeyEd25519 =
        new PrivateKeyEd25519(PrivateKey(sk.bytes.data.toArray), PublicKey(pk.bytes.data.toArray))
      secret -> secret.publicImage
    }

}

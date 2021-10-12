package co.topl.crypto.signatures

import co.topl.crypto.hash.sha256
import co.topl.crypto.typeclasses.implicits.{extendedEd25519ContainsVerificationKey, ContainsVerificationKeyOps}
import co.topl.crypto.{PrivateKey, PublicKey}
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.Sized
import co.topl.models.{Bytes, Proofs, SecretKeys, VerificationKeys}

import java.security.SecureRandom

class Ed25519 extends eddsa.Ed25519 with EllipticCurveSignatureScheme {
  override val SignatureLength: Int = SIGNATURE_SIZE
  override val KeyLength: Int = SECRET_KEY_SIZE

  override def createKeyPair(seed: Array[Byte]): (PrivateKey, PublicKey) = {
    val sk: Array[Byte] = new Array[Byte](SECRET_KEY_SIZE)
    val pk: Array[Byte] = new Array[Byte](PUBLIC_KEY_SIZE)
    val hashedSeed = sha256.hash(seed)
    val random = SecureRandom.getInstance("SHA1PRNG")

    random.setSeed(hashedSeed.value)
    generatePrivateKey(random, sk)
    generatePublicKey(sk, 0, pk, 0)
    (PrivateKey(sk), PublicKey(pk))
  }

  override def createKeyPair: (PrivateKey, PublicKey) = {
    val sk: Array[Byte] = new Array[Byte](SECRET_KEY_SIZE)
    val pk: Array[Byte] = new Array[Byte](PUBLIC_KEY_SIZE)
    val random = new SecureRandom()

    generatePrivateKey(random, sk)
    generatePublicKey(sk, 0, pk, 0)
    (PrivateKey(sk), PublicKey(pk))
  }

  override def sign(privateKey: PrivateKey, message: MessageToSign): Signature = {
    require(privateKey.value.length == SECRET_KEY_SIZE)
    val sig = new Array[Byte](SIGNATURE_SIZE)
    sign(privateKey.value, 0, message, 0, message.length, sig, 0)
    Signature(sig)
  }

  def sign(privateKey: Array[Byte], message: Array[Byte]): Signature = {
    require(privateKey.length == SECRET_KEY_SIZE)
    val sig = new Array[Byte](SIGNATURE_SIZE)
    sign(privateKey, 0, message, 0, message.length, sig, 0)
    Signature(sig)
  }

  override def verify(signature: Signature, message: MessageToSign, publicKey: PublicKey): Boolean =
    signature.value.length == SIGNATURE_SIZE &&
    publicKey.value.length == PUBLIC_KEY_SIZE &&
    verify(signature.value, 0, publicKey.value, 0, message, 0, message.length)

  def verify(signature: Array[Byte], message: Array[Byte], publicKey: Array[Byte]): Boolean =
    signature.length == SIGNATURE_SIZE &&
    publicKey.length == PUBLIC_KEY_SIZE &&
    verify(signature, 0, publicKey, 0, message, 0, message.length)

  def signExtended(t: SecretKeys.ExtendedEd25519, message: Array[Byte]): Proofs.Signature.Ed25519 = {
    implicit def ed25519: Ed25519 = this
    val signatureArray: Array[Byte] = new Array[Byte](SIGNATURE_SIZE)
    val ctx: Array[Byte] = Array.emptyByteArray
    val phflag: Byte = 0x00
    val h: Array[Byte] = (t.leftKey.data ++ t.rightKey.data).toArray
    val s: Array[Byte] = t.leftKey.data.toArray
    val vk = t.vk[VerificationKeys.ExtendedEd25519]
    val pk: Array[Byte] = (vk.ed25519.bytes.data ++ vk.chainCode.data).toArray
    implSign(sha512Digest, h, s, pk, 0, ctx, phflag, message, 0, message.length, signatureArray, 0)
    Proofs.Signature.Ed25519(Sized.strictUnsafe(Bytes(signatureArray)))
  }

}

object Ed25519 {
  val SignatureLength: Int = 64
  val KeyLength: Int = 32
}

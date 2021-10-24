package co.topl.crypto.signing

import co.topl.crypto.hash.sha256
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.Sized
import co.topl.models.{Bytes, Proofs, SecretKeys, VerificationKeys}

import java.security.SecureRandom

class Ed25519
    extends eddsa.Ed25519
    with EllipticCurveSignatureScheme[SecretKeys.Ed25519, VerificationKeys.Ed25519, Proofs.Signature.Ed25519] {
  override val SignatureLength: Int = SIGNATURE_SIZE
  override val KeyLength: Int = SECRET_KEY_SIZE

  override def createKeyPair(seed: Bytes): (SecretKeys.Ed25519, VerificationKeys.Ed25519) = {
    val sk: Array[Byte] = new Array[Byte](SECRET_KEY_SIZE)
    val pk: Array[Byte] = new Array[Byte](SECRET_KEY_SIZE)
    val hashedSeed = sha256.hash(seed.toArray)

    // should we update this?
    // https://stackoverflow.com/questions/27622625/securerandom-with-nativeprng-vs-sha1prng/27638413
    val random = SecureRandom.getInstance("SHA1PRNG")

    random.setSeed(hashedSeed.value)
    generatePrivateKey(random, sk)
    generatePublicKey(sk, 0, pk, 0)

    (SecretKeys.Ed25519(Sized.strictUnsafe(Bytes(sk))), VerificationKeys.Ed25519(Sized.strictUnsafe(Bytes(pk))))
  }

  override def sign(privateKey: SecretKeys.Ed25519, message: Bytes): Proofs.Signature.Ed25519 = {
    val sig = new Array[Byte](SIGNATURE_SIZE)
    sign(
      privateKey.bytes.data.toArray,
      0,
      message.toArray,
      0,
      message.toArray.length,
      sig,
      0
    )

    Proofs.Signature.Ed25519(Sized.strictUnsafe(Bytes(sig)))
  }

  override def verify(
    signature: Proofs.Signature.Ed25519,
    message:   Bytes,
    publicKey: VerificationKeys.Ed25519
  ): Boolean = {
    val sigByteArray = signature.bytes.data.toArray
    val vkByteArray = publicKey.bytes.data.toArray
    val msgByteArray = message.toArray

    sigByteArray.length == SIGNATURE_SIZE &&
    vkByteArray.length == PUBLIC_KEY_SIZE &&
    verify(
      sigByteArray,
      0,
      vkByteArray,
      0,
      msgByteArray,
      0,
      msgByteArray.length
    )
  }

  def generatePublicKey(secretKey: SecretKeys.Ed25519): VerificationKeys.Ed25519 = {
    val pkBytes = new Array[Byte](PUBLIC_KEY_SIZE)
    generatePublicKey(secretKey.bytes.data.toArray, 0, pkBytes, 0)
    VerificationKeys.Ed25519(Sized.strictUnsafe(Bytes(pkBytes)))
  }
}

object Ed25519 {
  val instance = new Ed25519
  instance.precompute()
}

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

  override def createKeyPair(seed: Seed): (SecretKeys.Ed25519, VerificationKeys.Ed25519) = {
    val sk: Sized.Strict[Bytes, SecretKeys.Ed25519.Length] =
      Sized.strictUnsafe(Bytes(new Array[Byte](SECRET_KEY_SIZE)))
    val pk: Sized.Strict[Bytes, VerificationKeys.Ed25519.Length] =
      Sized.strictUnsafe(Bytes(new Array[Byte](PUBLIC_KEY_SIZE)))
    val hashedSeed = sha256.hash(seed.value)

    // should we update this?
    // https://stackoverflow.com/questions/27622625/securerandom-with-nativeprng-vs-sha1prng/27638413
    val random = SecureRandom.getInstance("SHA1PRNG")

    random.setSeed(hashedSeed.value)
    generatePrivateKey(random, Bytes.toByteArray(sk.data))
    generatePublicKey(Bytes.toByteArray(sk.data), 0, Bytes.toByteArray(pk.data), 0)
    (SecretKeys.Ed25519(sk), VerificationKeys.Ed25519(pk))
  }

  override def sign(privateKey: SecretKeys.Ed25519, message: MessageToSign): Proofs.Signature.Ed25519 = {
    val sig: Sized.Strict[Bytes, Proofs.Signature.Ed25519.Length] =
      Sized.strictUnsafe(Bytes(new Array[Byte](SIGNATURE_SIZE)))
    sign(
      Bytes.toByteArray(privateKey.bytes.data),
      0,
      message.value,
      0,
      message.value.length,
      Bytes.toByteArray(sig.data),
      0
    )
    Proofs.Signature.Ed25519(sig)
  }

  override def verify(
    signature: Proofs.Signature.Ed25519,
    message:   MessageToSign,
    publicKey: VerificationKeys.Ed25519
  ): Boolean =
    signature.bytes.data.length == SIGNATURE_SIZE &&
    publicKey.bytes.data.length == PUBLIC_KEY_SIZE &&
    verify(
      Bytes.toByteArray(signature.bytes.data),
      0,
      Bytes.toByteArray(publicKey.bytes.data),
      0,
      message.value,
      0,
      message.value.length
    )

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

package co.topl.crypto.signing

import co.topl.crypto.hash.sha256
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.Sized
import co.topl.models.{Bytes, Proofs, SecretKeys, VerificationKeys}

import java.security.SecureRandom

class Ed25519VRF
    extends eddsa.ECVRF25519
    with EllipticCurveSignatureScheme[SecretKeys.VrfEd25519, VerificationKeys.VrfEd25519, Proofs.Signature.VrfEd25519] {
  override val SignatureLength: Int = SIGNATURE_SIZE
  override val KeyLength: Int = SECRET_KEY_SIZE

  override def createKeyPair(seed: Seed): (SecretKeys.VrfEd25519, VerificationKeys.VrfEd25519) = {
    val sk: Sized.Strict[Bytes, SecretKeys.VrfEd25519.Length] =
      Sized.strictUnsafe(Bytes(new Array[Byte](SECRET_KEY_SIZE)))
    val pk: Sized.Strict[Bytes, VerificationKeys.VrfEd25519.Length] =
      Sized.strictUnsafe(Bytes(new Array[Byte](PUBLIC_KEY_SIZE)))
    val hashedSeed = sha256.hash(seed.value)
    val random = SecureRandom.getInstance("SHA1PRNG")
    random.setSeed(hashedSeed.value)

    generatePrivateKey(random, Bytes.toByteArray(sk.data))
    generatePublicKey(Bytes.toByteArray(sk.data), 0, Bytes.toByteArray(pk.data), 0)
    (SecretKeys.VrfEd25519(sk), VerificationKeys.VrfEd25519(pk))
  }

  override def sign(privateKey: SecretKeys.VrfEd25519, message: MessageToSign): Proofs.Signature.VrfEd25519 =
    Proofs.Signature.VrfEd25519(
      Sized.strictUnsafe(
        Bytes(
          vrfProof(privateKey.bytes.data.toArray, message.value)
        )
      )
    )

  override def verify(
    signature: Proofs.Signature.VrfEd25519,
    message:   MessageToSign,
    publicKey: VerificationKeys.VrfEd25519
  ): Boolean = {
    signature.bytes.data.length == SIGNATURE_SIZE &&
    publicKey.bytes.data.length == PUBLIC_KEY_SIZE &&
    vrfVerify(Bytes.toByteArray(publicKey.bytes.data), message.value, Bytes.toByteArray(signature.bytes.data))
  }

  def generatePublicKey(secretKey: SecretKeys.VrfEd25519): VerificationKeys.VrfEd25519 = {
    val pkBytes = new Array[Byte](PUBLIC_KEY_SIZE)
    generatePublicKey(secretKey.bytes.data.toArray, 0, pkBytes, 0)
    VerificationKeys.VrfEd25519(Sized.strictUnsafe(Bytes(pkBytes)))
  }

}

object Ed25519VRF {
  val instance = new Ed25519VRF
  instance.precompute()
}

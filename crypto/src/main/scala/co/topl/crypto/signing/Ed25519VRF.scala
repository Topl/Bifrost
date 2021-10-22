package co.topl.crypto.signing

import co.topl.crypto.hash.sha256
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.{Lengths, Sized}
import co.topl.models.{Bytes, Proofs, SecretKeys, VerificationKeys}

import java.security.SecureRandom

class Ed25519VRF
    extends eddsa.ECVRF25519
    with EllipticCurveSignatureScheme[SecretKeys.VrfEd25519, VerificationKeys.VrfEd25519, Proofs.Signature.VrfEd25519] {
  override val SignatureLength: Int = SIGNATURE_SIZE
  override val KeyLength: Int = SECRET_KEY_SIZE

  override def createKeyPair(seed: Bytes): (SecretKeys.VrfEd25519, VerificationKeys.VrfEd25519) = {
    val sk: Sized.Strict[Bytes, SecretKeys.VrfEd25519.Length] =
      Sized.strictUnsafe(Bytes(new Array[Byte](SECRET_KEY_SIZE)))
    val pk: Sized.Strict[Bytes, VerificationKeys.VrfEd25519.Length] =
      Sized.strictUnsafe(Bytes(new Array[Byte](PUBLIC_KEY_SIZE)))
    val hashedSeed = sha256.hash(seed.toArray)
    val random = SecureRandom.getInstance("SHA1PRNG")
    random.setSeed(hashedSeed.value)

    generatePrivateKey(random, sk.data.toArray)
    generatePublicKey(sk.data.toArray, 0, pk.data.toArray, 0)
    (SecretKeys.VrfEd25519(sk), VerificationKeys.VrfEd25519(pk))
  }

  override def sign(privateKey: SecretKeys.VrfEd25519, message: Bytes): Proofs.Signature.VrfEd25519 =
    Proofs.Signature.VrfEd25519(
      Sized.strictUnsafe(
        Bytes(
          vrfProof(privateKey.bytes.data.toArray, message.toArray)
        )
      )
    )

  override def verify(
    signature: Proofs.Signature.VrfEd25519,
    message:   Bytes,
    publicKey: VerificationKeys.VrfEd25519
  ): Boolean =
    vrfVerify(publicKey.bytes.data.toArray, message.toArray, signature.bytes.data.toArray)

  def generatePublicKey(secretKey: SecretKeys.VrfEd25519): VerificationKeys.VrfEd25519 = {
    val pkBytes = new Array[Byte](PUBLIC_KEY_SIZE)
    generatePublicKey(secretKey.bytes.data.toArray, 0, pkBytes, 0)
    VerificationKeys.VrfEd25519(Sized.strictUnsafe(Bytes(pkBytes)))
  }

  def proofToHash(signature: Proofs.Signature.VrfEd25519): Sized.Strict[Bytes, Lengths.`64`.type] =
    Sized.strictUnsafe(Bytes(vrfProofToHash(signature.bytes.data.toArray)))

}

object Ed25519VRF {
  val instance = new Ed25519VRF
  instance.precompute()
}

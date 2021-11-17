package co.topl.crypto.signing

import co.topl.crypto.signing.eddsa.ECVRF25519
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.{Lengths, Sized}
import co.topl.models.{Bytes, Proofs, SecretKeys, VerificationKeys}

class Ed25519VRF
    extends EllipticCurveSignatureScheme[
      SecretKeys.VrfEd25519,
      VerificationKeys.VrfEd25519,
      Proofs.Signature.VrfEd25519,
      SecretKeys.VrfEd25519.Length
    ] {

  private val impl = new ECVRF25519
  impl.precompute()

  override val SignatureLength: Int = impl.SIGNATURE_SIZE
  override val KeyLength: Int = impl.SECRET_KEY_SIZE

  override protected def createKeyPair(
    seed: Sized.Strict[Bytes, SecretKeys.VrfEd25519.Length]
  ): (SecretKeys.VrfEd25519, VerificationKeys.VrfEd25519) = {
    val sk = new Array[Byte](impl.SECRET_KEY_SIZE)
    val pk = new Array[Byte](impl.PUBLIC_KEY_SIZE)

    val random = defaultRandom(Some(Seed(seed.data.toArray)))

    impl.generatePrivateKey(random, sk)
    impl.generatePublicKey(sk, 0, pk, 0)
    (SecretKeys.VrfEd25519(Sized.strictUnsafe(Bytes(sk))), VerificationKeys.VrfEd25519(Sized.strictUnsafe(Bytes(pk))))
  }

  override def sign(privateKey: SecretKeys.VrfEd25519, message: Bytes): Proofs.Signature.VrfEd25519 =
    Proofs.Signature.VrfEd25519(
      Sized.strictUnsafe(
        Bytes(
          impl.vrfProof(privateKey.bytes.data.toArray, message.toArray)
        )
      )
    )

  override def verify(
    signature: Proofs.Signature.VrfEd25519,
    message:   Bytes,
    publicKey: VerificationKeys.VrfEd25519
  ): Boolean =
    impl.vrfVerify(publicKey.bytes.data.toArray, message.toArray, signature.bytes.data.toArray)

  def getVerificationKey(secretKey: SecretKeys.VrfEd25519): VerificationKeys.VrfEd25519 = {
    val pkBytes = new Array[Byte](impl.PUBLIC_KEY_SIZE)
    impl.generatePublicKey(secretKey.bytes.data.toArray, 0, pkBytes, 0)
    VerificationKeys.VrfEd25519(Sized.strictUnsafe(Bytes(pkBytes)))
  }

  def proofToHash(signature: Proofs.Signature.VrfEd25519): Sized.Strict[Bytes, Lengths.`64`.type] =
    Sized.strictUnsafe(Bytes(impl.vrfProofToHash(signature.bytes.data.toArray)))

}

object Ed25519VRF {

  def precomputed(): Ed25519VRF =
    new Ed25519VRF
}

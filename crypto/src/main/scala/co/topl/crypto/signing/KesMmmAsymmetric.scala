package co.topl.crypto.signing

import co.topl.models.Proofs.Signature
import co.topl.models.{Proofs, SecretKeys, VerificationKeys}

class KesMmmAsymmetric
    extends kes.KesEd25519Blake2b256
    with EllipticCurveSignatureScheme[
      SecretKeys.KesAsymmetricProduct,
      VerificationKeys.KesAsymmetricProduct,
      Proofs.Signature.KesAsymmetricProduct
    ]
    with EvolvingSignatureScheme[
      SecretKeys.KesAsymmetricProduct,
      VerificationKeys.KesAsymmetricProduct,
      Long
    ] {
  override val hash: Nothing = _
  override val sig: Nothing = _
  override val seedBytes: Int = _
  override val pkBytes: Int = _
  override val skBytes: Int = _
  override val sigBytes: Int = _
  override val hashBytes: Int = _
  override val pkLength: Int = _
  override val asymmetricLogL: Int = _
  override val symmetricLogL: Int = _
  override val SignatureLength: Int = _
  override val KeyLength: Int = _

  override def createKeyPair(seed: Seed): (SecretKeys.KesAsymmetricProduct, VerificationKeys.KesAsymmetricProduct) = ???

  override def createKeyPair: (SecretKeys.KesAsymmetricProduct, VerificationKeys.KesAsymmetricProduct) = ???

  override def sign(privateKey: SecretKeys.KesAsymmetricProduct, message: MessageToSign): Signature.KesAsymmetricProduct = ???

  override def verify(signature: Signature.KesAsymmetricProduct, message: MessageToSign, verifyKey: VerificationKeys.KesAsymmetricProduct): Boolean = ???

  override def deriveSecret(secretKey: SecretKeys.KesAsymmetricProduct, index: Long): SecretKeys.KesAsymmetricProduct = ???

  override def deriveVerification(verificationKey: VerificationKeys.KesAsymmetricProduct, index: Long): VerificationKeys.KesAsymmetricProduct = ???
}

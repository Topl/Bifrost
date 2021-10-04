package co.topl.crypto.signing

import co.topl.models.Proofs.Signature
import co.topl.models.{Proofs, SecretKeys, VerificationKeys}

class KesMmmSymmetricProduct
    extends kes.MMM
    with EllipticCurveSignatureScheme[
      SecretKeys.KesMmmSymmetricProduct,
      VerificationKeys.KesMmmSymmetricProduct,
      Proofs.Signature.KesMmmSymmetricProduct
    ]
    with EvolvingSignatureScheme[
      SecretKeys.KesMmmSymmetricProduct,
      VerificationKeys.KesMmmSymmetricProduct,
      Long
    ] {
  override val fch: Nothing = _
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

  override def createKeyPair(seed: Seed): (SecretKeys.KesMmmSymmetricProduct, VerificationKeys.KesMmmSymmetricProduct) = ???

  override def createKeyPair: (SecretKeys.KesMmmSymmetricProduct, VerificationKeys.KesMmmSymmetricProduct) = ???

  override def sign(privateKey: SecretKeys.KesMmmSymmetricProduct, message: MessageToSign): Signature.KesMmmSymmetricProduct = ???

  override def verify(signature: Signature.KesMmmSymmetricProduct, message: MessageToSign, verifyKey: VerificationKeys.KesMmmSymmetricProduct): Boolean = ???

  override def deriveSecret(secretKey: SecretKeys.KesMmmSymmetricProduct, index: Long): SecretKeys.KesMmmSymmetricProduct = ???

  override def deriveVerification(verificationKey: VerificationKeys.KesMmmSymmetricProduct, index: Long): VerificationKeys.KesMmmSymmetricProduct = ???
}

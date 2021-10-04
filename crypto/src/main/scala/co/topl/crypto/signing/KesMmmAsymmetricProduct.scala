package co.topl.crypto.signing

import co.topl.models.Proofs.Signature
import co.topl.models.{Proofs, SecretKeys, VerificationKeys}

class KesMmmAsymmetricProduct
    extends kes.MMM
    with EllipticCurveSignatureScheme[
      SecretKeys.KesMmmAsymmetricProduct,
      VerificationKeys.KesMmmAsymmetricProduct,
      Proofs.Signature.KesMmmAsymmetricProduct
    ]
    with EvolvingSignatureScheme[
      SecretKeys.KesMmmAsymmetricProduct,
      VerificationKeys.KesMmmAsymmetricProduct,
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

  override def createKeyPair(seed: Seed): (SecretKeys.KesMmmAsymmetricProduct, VerificationKeys.KesMmmAsymmetricProduct) = ???

  override def createKeyPair: (SecretKeys.KesMmmAsymmetricProduct, VerificationKeys.KesMmmAsymmetricProduct) = ???

  override def sign(privateKey: SecretKeys.KesMmmAsymmetricProduct, message: MessageToSign): Signature.KesMmmAsymmetricProduct = ???

  override def verify(signature: Signature.KesMmmAsymmetricProduct, message: MessageToSign, verifyKey: VerificationKeys.KesMmmAsymmetricProduct): Boolean = ???

  override def deriveSecret(secretKey: SecretKeys.KesMmmAsymmetricProduct, index: Long): SecretKeys.KesMmmAsymmetricProduct = ???

  override def deriveVerification(verificationKey: VerificationKeys.KesMmmAsymmetricProduct, index: Long): VerificationKeys.KesMmmAsymmetricProduct = ???
}

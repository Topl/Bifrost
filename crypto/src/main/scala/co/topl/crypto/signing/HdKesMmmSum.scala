package co.topl.crypto.signing

import co.topl.models.Proofs.Signature
import co.topl.models.{Proofs, SecretKeys, VerificationKeys}

class HdKesMmmSum
    extends kes.MMM
    with EllipticCurveSignatureScheme[
      SecretKeys.HdKesMmmSum,
      VerificationKeys.HdKesMmmSum,
      Proofs.Signature.HdKesMmmSum
    ]
    with EvolvingSignatureScheme[
      SecretKeys.HdKesMmmSum,
      VerificationKeys.HdKesMmmSum,
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

  override def createKeyPair(seed: Seed): (SecretKeys.HdKesMmmSum, VerificationKeys.HdKesMmmSum) = ???

  override def createKeyPair: (SecretKeys.HdKesMmmSum, VerificationKeys.HdKesMmmSum) = ???

  override def sign(privateKey: SecretKeys.HdKesMmmSum, message: MessageToSign): Signature.HdKesMmmSum = ???

  override def verify(signature: Signature.HdKesMmmSum, message: MessageToSign, verifyKey: VerificationKeys.HdKesMmmSum): Boolean = ???

  override def deriveSecret(secretKey: SecretKeys.HdKesMmmSum, index: Long): SecretKeys.HdKesMmmSum = ???

  override def deriveVerification(verificationKey: VerificationKeys.HdKesMmmSum, index: Long): VerificationKeys.HdKesMmmSum = ???
}

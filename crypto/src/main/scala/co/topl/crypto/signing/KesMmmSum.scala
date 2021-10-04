package co.topl.crypto.signing

import co.topl.crypto.signing.bip32ed25519.KeyIndex
import co.topl.models.Proofs.Signature
import co.topl.models.{Proofs, SecretKeys, VerificationKeys}

class KesMmmSum
  extends kes.MMM
    with EllipticCurveSignatureScheme[
    SecretKeys.KesMmmSum,
    VerificationKeys.KesMmmSum,
    Proofs.Signature.KesMmmSum
  ]
    with EvolvingSignatureScheme[
    SecretKeys.KesMmmSum,
    VerificationKeys.KesMmmSum,
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

  override def createKeyPair(seed: Seed): (SecretKeys.KesMmmSum, VerificationKeys.KesMmmSum) = ???

  override def createKeyPair: (SecretKeys.KesMmmSum, VerificationKeys.KesMmmSum) = ???

  override def sign(privateKey: SecretKeys.KesMmmSum, message: MessageToSign): Signature.KesMmmSum = ???

  override def verify(signature: Signature.KesMmmSum, message: MessageToSign, verifyKey: VerificationKeys.KesMmmSum): Boolean = ???

  override def deriveSecret(secretKey: SecretKeys.KesMmmSum, index: Long): SecretKeys.KesMmmSum = ???

  override def deriveVerification(verificationKey: VerificationKeys.KesMmmSum, index: Long): VerificationKeys.KesMmmSum = ???
}

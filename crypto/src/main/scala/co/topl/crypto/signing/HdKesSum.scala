package co.topl.crypto.signing

import co.topl.models.Proofs.Signature
import co.topl.models.{Proofs, SecretKeys, VerificationKeys}

class HdKesSum
    extends kes.KesEd25519Blake2b256
    with EllipticCurveSignatureScheme[
      SecretKeys.HdKesSum,
      VerificationKeys.HdKesSum,
      Proofs.Signature.HdKesSum
    ]
    with EvolvingSignatureScheme[
      SecretKeys.HdKesSum,
      VerificationKeys.HdKesSum,
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

  override def createKeyPair(seed: Seed): (SecretKeys.HdKesSum, VerificationKeys.HdKesSum) = ???

  override def createKeyPair: (SecretKeys.HdKesSum, VerificationKeys.HdKesSum) = ???

  override def sign(privateKey: SecretKeys.HdKesSum, message: MessageToSign): Signature.HdKesSum = ???

  override def verify(signature: Signature.HdKesSum, message: MessageToSign, verifyKey: VerificationKeys.HdKesSum): Boolean = ???

  override def deriveSecret(secretKey: SecretKeys.HdKesSum, index: Long): SecretKeys.HdKesSum = ???

  override def deriveVerification(verificationKey: VerificationKeys.HdKesSum, index: Long): VerificationKeys.HdKesSum = ???
}

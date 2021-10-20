package co.topl.crypto.signing

import co.topl.crypto.signing.kes.SumComposition
import co.topl.models.{Proofs, SecretKeys, VerificationKeys}

class KesSum
    extends SumComposition
    with KeyEvolvingSignatureScheme[SecretKeys.KesSum, VerificationKeys.KesSum, Proofs.Signature.KesSum, Int] {

  override val defaultHeight: Int = 7
  override val defaultOffset: Long = 0

  def createKeyPair(seed: Seed, height: Int): (SecretKeys.KesSum, VerificationKeys.KesSum) = {
    val sk = generateSecretKey(seed.value, height)
    val pk = generateVerificationKey(sk)

    // todo: fix offset
  ???
  }

  def sign(privateKey: SecretKeys.KesSum, message: MessageToSign, index: Int): Proofs.Signature.KesSum =
    ???
  ///Signature.KesSum(sign(privateKey.tree, message.value))

  def verify(
    signature: Proofs.Signature.KesSum,
    message:   MessageToSign,
    verifyKey: VerificationKeys.KesSum,
    index:     Int
  ): Boolean = ???

  def deriveSecret(secretKey: SecretKeys.KesSum, index: Int): SecretKeys.KesSum =
    SecretKeys.KesSum(updateKey(secretKey.tree, index), secretKey.offset)

  def deriveVerification(
    verificationKey: VerificationKeys.KesSum,
    index:           Long
  ): VerificationKeys.KesSum = ???

  override def sign(privateKey: SecretKeys.KesSum, message: MessageToSign): Proofs.Signature.KesSum = ???

  override def verify(
    signature: Proofs.Signature.KesSum,
    message:   MessageToSign,
    verifyKey: VerificationKeys.KesSum
  ): Boolean = ???

  override def createKeyPair(seed: Seed, height: Int, offset: Long): (SecretKeys.KesSum, VerificationKeys.KesSum) = ???

  override def update(privateKey: SecretKeys.KesSum, steps: Long): SecretKeys.KesSum = ???
}

object KesSum {
  val instance: KesSum = new KesSum
}

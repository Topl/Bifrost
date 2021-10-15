package co.topl.crypto.signing

import co.topl.crypto.signing.kes.SumComposition
import co.topl.models.Proofs.Signature
import co.topl.models.{SecretKeys, VerificationKeys}

class KesSum extends SumComposition {

//  override val SignatureLength: Int = ???
//  override val KeyLength: Int = ???
  val defaultHeight: Int = ???

  def createKeyPair(seed: Seed, height: Int): (SecretKeys.KesSum, VerificationKeys.KesSum) = {
    val sk = generateSecretKey(seed.value, height)
    val pk = generateVerificationKey(sk)

    // todo: fix offset
    (SecretKeys.KesSum(sk, 0), VerificationKeys.KesSum(pk))
  }

  def sign(privateKey: SecretKeys.KesSum, message: MessageToSign, index: Int): Signature.KesSum =
    Signature.KesSum(sign(privateKey.tree, message.value, index))

  def verify(
    signature: Signature.KesSum,
    message:   MessageToSign,
    verifyKey: VerificationKeys.KesSum,
    index:     Int
  ): Boolean = verify(verifyKey.bytes, message.value, signature.bytes, index: Int)

  def deriveSecret(secretKey: SecretKeys.KesSum, index: Int): SecretKeys.KesSum =
    SecretKeys.KesSum(updateKey(secretKey.tree, index), secretKey.offset)

  def deriveVerification(
    verificationKey: VerificationKeys.KesSum,
    index:           Long
  ): VerificationKeys.KesSum = ???

//  override def createKeyPair(seed: Seed): (SecretKeys.KesSum, VerificationKeys.KesSum) =
//    createKeyPair(seed, defaultHeight)
//
//  override def createKeyPair: (SecretKeys.KesSum, VerificationKeys.KesSum) = {
//    val random = SecureRandom.getInstance("SHA1PRNG")
//    createKeyPair(Seed(random.generateSeed(128)))
//  }
//
//  override def sign(privateKey: SecretKeys.KesSum, message: MessageToSign): Signature.KesSum = ???
//
//  override def verify(
//    signature: Signature.KesSum,
//    message:   MessageToSign,
//    verifyKey: VerificationKeys.KesSum
//  ): Boolean = ???
//
//  override def deriveSecret(secretKey: SecretKeys.KesSum, index: Long): SecretKeys.KesSum = ???
//
//  override def deriveVerification(
//    verificationKey: VerificationKeys.KesSum,
//    index:           Long
//  ): VerificationKeys.KesSum = ???

}

object KesSum {
  val instance: KesSum = new KesSum
}

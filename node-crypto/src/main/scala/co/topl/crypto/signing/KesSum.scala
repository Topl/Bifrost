package co.topl.crypto.signing

import co.topl.crypto.models._
import co.topl.crypto.signing.kes.SumComposition

/**
 * Credit to Aaron Schutza
 */
class KesSum extends SumComposition {

  def createKeyPair(seed: Array[Byte], height: Int, offset: Long): (SecretKeyKesSum, VerificationKeyKesSum) = {
    val sk: KesBinaryTree = generateSecretKey(seed.toArray, height)
    val pk: (Array[Byte], Int) = generateVerificationKey(sk)
    (SecretKeyKesSum(sk, offset), VerificationKeyKesSum(pk._1, pk._2))
  }

  def sign(privateKey: SecretKeyKesSum, message: Array[Byte]): SignatureKesSum = {
    val sumSig = sign(privateKey.tree, message.toArray)
    SignatureKesSum(
      sumSig._1,
      sumSig._2,
      sumSig._3
    )
  }

  def verify(
    signature: SignatureKesSum,
    message:   Array[Byte],
    verifyKey: VerificationKeyKesSum
  ): Boolean = {
    val sumSig =
      (
        signature.verificationKey,
        signature.signature,
        signature.witness.toVector
      )
    val sumVk = (verifyKey.value, verifyKey.step)
    verify(sumSig, message, sumVk)
  }

  def update(privateKey: SecretKeyKesSum, steps: Int): SecretKeyKesSum =
    privateKey.copy(tree = updateKey(privateKey.tree, steps))

  def getCurrentStep(privateKay: SecretKeyKesSum): Int = getKeyTime(privateKay.tree)

  def getMaxStep(privateKay: SecretKeyKesSum): Int = exp(getTreeHeight(privateKay.tree))

  def getVerificationKey(privateKey: SecretKeyKesSum): VerificationKeyKesSum = {
    val vk = generateVerificationKey(privateKey.tree)
    VerificationKeyKesSum(vk._1, vk._2)
  }
}

object KesSum {
  val instance: KesSum = new KesSum
}

package co.topl.crypto.signing

import co.topl.crypto.signing.kes.SumComposition
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.{KesBinaryTree, Sized}
import co.topl.models.{Bytes, Proofs, SecretKeys, VerificationKeys}

class KesSum
    extends SumComposition
    with KeyEvolvingSignatureScheme[SecretKeys.KesSum, VerificationKeys.KesSum, Proofs.Knowledge.KesSum, Int] {

  override def createKeyPair(seed: Bytes, height: Int, offset: Long): (SecretKeys.KesSum, VerificationKeys.KesSum) = {
    val sk: KesBinaryTree = generateSecretKey(seed.toArray, height)
    val pk: (Array[Byte], Int) = generateVerificationKey(sk)
    (SecretKeys.KesSum(sk, offset), VerificationKeys.KesSum(Sized.strictUnsafe(Bytes(pk._1)), pk._2))
  }

  override def sign(privateKey: SecretKeys.KesSum, message: Bytes): Proofs.Knowledge.KesSum = {
    val sumSig = sign(privateKey.tree, message.toArray)
    Proofs.Knowledge.KesSum(
      VerificationKeys.Ed25519(Sized.strictUnsafe(Bytes(sumSig._1))),
      Proofs.Knowledge.Ed25519(Sized.strictUnsafe(Bytes(sumSig._2))),
      sumSig._3.map(w => Sized.strictUnsafe(Bytes(w)))
    )
  }

  override def verify(
    signature: Proofs.Knowledge.KesSum,
    message:   Bytes,
    verifyKey: VerificationKeys.KesSum
  ): Boolean = {
    val sumSig =
      (
        signature.verificationKey.bytes.data.toArray,
        signature.signature.bytes.data.toArray,
        signature.witness.map(_.data.toArray)
      )
    val sumVk = (verifyKey.bytes.data.toArray, verifyKey.step)
    verify(sumSig, message.toArray, sumVk)
  }

  override def update(privateKey: SecretKeys.KesSum, steps: Int): SecretKeys.KesSum =
    privateKey.copy(tree = updateKey(privateKey.tree, steps))

  override def getCurrentStep(privateKay: SecretKeys.KesSum): Int = getKeyTime(privateKay.tree)

  override def getMaxStep(privateKay: SecretKeys.KesSum): Int = exp(getTreeHeight(privateKay.tree))

  override def getVerificationKey(privateKey: SecretKeys.KesSum): VerificationKeys.KesSum = {
    val vk = generateVerificationKey(privateKey.tree)
    VerificationKeys.KesSum(Sized.strictUnsafe(Bytes(vk._1)), vk._2)
  }
}

object KesSum {
  val instance: KesSum = new KesSum
}

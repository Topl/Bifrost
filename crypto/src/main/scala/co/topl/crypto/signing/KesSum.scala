package co.topl.crypto.signing

import co.topl.crypto.signing.kes.SumComposition
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.{KesBinaryTree, Sized}
import co.topl.models.{Bytes, Proofs, SecretKeys, VerificationKeys}

class KesSum
    extends SumComposition
    with KeyEvolvingSignatureScheme[SecretKeys.KesSum, VerificationKeys.KesSum, Proofs.Signature.KesSum, Int] {

  override def createKeyPair(seed: Seed, height: Int, offset: Int): (SecretKeys.KesSum, VerificationKeys.KesSum) = {
    val sk: KesBinaryTree = generateSecretKey(seed.value, height)
    val pk: (Array[Byte], Int) = generateVerificationKey(sk)
    (SecretKeys.KesSum(sk, offset), VerificationKeys.KesSum(Sized.strictUnsafe(Bytes(pk._1)), pk._2))
  }

  override def sign(privateKey: SecretKeys.KesSum, message: MessageToSign): Proofs.Signature.KesSum = {
    val sumSig = sign(privateKey.tree, message.value)
    Proofs.Signature.KesSum(
      VerificationKeys.Ed25519(Sized.strictUnsafe(Bytes(sumSig._1))),
      Proofs.Signature.Ed25519(Sized.strictUnsafe(Bytes(sumSig._2))),
      sumSig._3.map(w => Sized.strictUnsafe(Bytes(w)))
    )
  }

  override def verify(
    signature: Proofs.Signature.KesSum,
    message:   MessageToSign,
    verifyKey: VerificationKeys.KesSum
  ): Boolean = {
    val sumSig =
      (signature.vk.bytes.data.toArray, signature.sig.bytes.data.toArray, signature.witness.map(_.data.toArray))
    val sumVk = (verifyKey.bytes.data.toArray, verifyKey.step)
    verify(sumSig, message.value, sumVk)
  }

  override def update(privateKey: SecretKeys.KesSum, steps: Int): SecretKeys.KesSum =
    privateKey.copy(tree = updateKey(privateKey.tree, steps))

  override def getKeyTime(privateKay: SecretKeys.KesSum): Int = getKeyTime(privateKay.tree)

  override def getVerificationKey(privateKey: SecretKeys.KesSum): VerificationKeys.KesSum = {
    val vk = generateVerificationKey(privateKey.tree)
    VerificationKeys.KesSum(Sized.strictUnsafe(Bytes(vk._1)), vk._2)
  }
}

object KesSum {
  val instance: KesSum = new KesSum
}
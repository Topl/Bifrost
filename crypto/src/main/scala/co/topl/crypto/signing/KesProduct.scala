package co.topl.crypto.signing

import co.topl.crypto.signing.kes.ProductComposition
import co.topl.models.Proofs.Signature
import co.topl.models.utility.{KesBinaryTree, Sized}
import co.topl.models.{Bytes, Proofs, SecretKeys, VerificationKeys}

class KesProduct
    extends ProductComposition
    with KeyEvolvingSignatureScheme[
      SecretKeys.KesProduct,
      VerificationKeys.KesProduct,
      Proofs.Signature.KesProduct,
      (Int, Int)
    ] {

  override def createKeyPair(
    seed:   Seed,
    height: (Int, Int),
    offset: Int
  ): (SecretKeys.KesProduct, VerificationKeys.KesProduct) = {
    ???
//    val sk = generateSecretKey(seed.value, height._1, height._2)
//    val pk = generateVerificationKey(sk)
//    (SecretKeys.KesProduct(sk, offset),
//      VerificationKeys.KesProduct(Sized.strictUnsafe(Bytes(pk._1)), pk._2))
  }

  override def sign(privateKey: SecretKeys.KesProduct, message: MessageToSign): Signature.KesProduct = ???

  override def verify(
    signature: Signature.KesProduct,
    message:   MessageToSign,
    verifyKey: VerificationKeys.KesProduct
  ): Boolean = ???

  override def update(privateKey: SecretKeys.KesProduct, steps: Int): SecretKeys.KesProduct = ???

  override def getCurrentStep(privateKay: SecretKeys.KesProduct): Int = ???

  override def getMaxStep(privateKey: SecretKeys.KesProduct): Int = ???

  override def getVerificationKey(privateKey: SecretKeys.KesProduct): VerificationKeys.KesProduct = ???
}

object KesProduct {
  val instance: KesProduct = new KesProduct
}

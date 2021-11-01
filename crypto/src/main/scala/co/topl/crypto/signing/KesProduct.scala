package co.topl.crypto.signing

import co.topl.crypto.signing.kes.ProductComposition
import co.topl.models.Proofs.Signature
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Sized
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
    seed:   Bytes,
    height: (Int, Int),
    offset: Long
  ): (SecretKeys.KesProduct, VerificationKeys.KesProduct) = {
    val sk = generateSecretKey(seed.toArray, height._1, height._2)
    val pk = generateVerificationKey(sk)
    (
      SecretKeys.KesProduct(
        sk._1,
        sk._2,
        Sized.strictUnsafe(Bytes(sk._3)),
        Proofs.Signature.KesSum(
          VerificationKeys.Ed25519(Sized.strictUnsafe(Bytes(sk._4._1))),
          Proofs.Signature.Ed25519(Sized.strictUnsafe(Bytes(sk._4._2))),
          sk._4._3.map(w => Sized.strictUnsafe(Bytes(w)))
        ),
        offset
      ),
      VerificationKeys.KesProduct(Sized.strictUnsafe(Bytes(pk._1)), pk._2)
    )
  }

  override def sign(privateKey: SecretKeys.KesProduct, message: Bytes): Signature.KesProduct = {
    val prodSig = sign(unpackSecret(privateKey), message.toArray)

    Proofs.Signature.KesProduct(
      Proofs.Signature.KesSum(
        VerificationKeys.Ed25519(Sized.strictUnsafe(Bytes(prodSig._1._1))),
        Proofs.Signature.Ed25519(Sized.strictUnsafe(Bytes(prodSig._1._2))),
        prodSig._1._3.map(w => Sized.strictUnsafe(Bytes(w)))
      ),
      Proofs.Signature.KesSum(
        VerificationKeys.Ed25519(Sized.strictUnsafe(Bytes(prodSig._2._1))),
        Proofs.Signature.Ed25519(Sized.strictUnsafe(Bytes(prodSig._2._2))),
        prodSig._2._3.map(w => Sized.strictUnsafe(Bytes(w)))
      ),
      Sized.strictUnsafe(Bytes(prodSig._3))
    )
  }

  override def verify(
    signature: Signature.KesProduct,
    message:   Bytes,
    verifyKey: VerificationKeys.KesProduct
  ): Boolean = {
    val prodSig = (
      (
        signature.superSignature.verificationKey.bytes.data.toArray,
        signature.superSignature.signature.bytes.data.toArray,
        signature.superSignature.witness.map(_.data.toArray)
      ),
      (
        signature.subSignature.verificationKey.bytes.data.toArray,
        signature.subSignature.signature.bytes.data.toArray,
        signature.subSignature.witness.map(_.data.toArray)
      ),
      signature.subRoot.data.toArray
    )

    val sumVk = (verifyKey.bytes.data.toArray, verifyKey.step)
    verify(prodSig, message.toArray, sumVk)
  }

  override def update(privateKey: SecretKeys.KesProduct, steps: Int): SecretKeys.KesProduct = {
    val sk = updateKey(unpackSecret(privateKey), steps)

    SecretKeys.KesProduct(
      sk._1,
      sk._2,
      Sized.strictUnsafe(Bytes(sk._3)),
      Proofs.Signature.KesSum(
        VerificationKeys.Ed25519(Sized.strictUnsafe(Bytes(sk._4._1))),
        Proofs.Signature.Ed25519(Sized.strictUnsafe(Bytes(sk._4._2))),
        sk._4._3.map(w => Sized.strictUnsafe(Bytes(w)))
      ),
      privateKey.offset
    )
  }

  override def getCurrentStep(privateKay: SecretKeys.KesProduct): Int = getKeyTime(unpackSecret(privateKay))

  override def getMaxStep(privateKey: SecretKeys.KesProduct): Int = exp(
    getTreeHeight(privateKey.superTree) + getTreeHeight(privateKey.subTree)
  )

  override def getVerificationKey(privateKey: SecretKeys.KesProduct): VerificationKeys.KesProduct = {
    val vk = generateVerificationKey(unpackSecret(privateKey))
    VerificationKeys.KesProduct(Sized.strictUnsafe(Bytes(vk._1)), vk._2)
  }

  private def unpackSecret(privateKey: SecretKeys.KesProduct): SK =
    (
      privateKey.superTree,
      privateKey.subTree,
      privateKey.nextSubSeed.data.toArray,
      (
        privateKey.subSignature.verificationKey.bytes.data.toArray,
        privateKey.subSignature.signature.bytes.data.toArray,
        privateKey.subSignature.witness.map(_.data.toArray)
      )
    )
}

object KesProduct {
  val instance: KesProduct = new KesProduct
}

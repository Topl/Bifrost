package co.topl.crypto.signing

import co.topl.crypto.models._
import co.topl.crypto.signing.kes.ProductComposition

class KesProduct extends ProductComposition {

  def createKeyPair(
    seed:   Array[Byte],
    height: (Int, Int),
    offset: Long
  ): (SecretKeyKesProduct, VerificationKeyKesProduct) = {
    val sk = generateSecretKey(seed, height._1, height._2)
    val pk = generateVerificationKey(sk)
    (
      SecretKeyKesProduct(
        sk._1,
        sk._2,
        sk._3,
        SignatureKesSum(
          sk._4._1,
          sk._4._2,
          sk._4._3
        ),
        offset
      ),
      VerificationKeyKesProduct(pk._1, pk._2)
    )
  }

  def sign(privateKey: SecretKeyKesProduct, message: Array[Byte]): SignatureKesProduct = {
    val prodSig = sign(unpackSecret(privateKey), message)

    SignatureKesProduct(
      SignatureKesSum(
        prodSig._1._1,
        prodSig._1._2,
        prodSig._1._3
      ),
      SignatureKesSum(
        prodSig._2._1,
        prodSig._2._2,
        prodSig._2._3
      ),
      prodSig._3
    )
  }

  def verify(
    signature: SignatureKesProduct,
    message:   Array[Byte],
    verifyKey: VerificationKeyKesProduct
  ): Boolean = {
    val prodSig = (
      (
        signature.superSignature.verificationKey,
        signature.superSignature.signature,
        signature.superSignature.witness.toVector
      ),
      (
        signature.subSignature.verificationKey,
        signature.subSignature.signature,
        signature.subSignature.witness.toVector
      ),
      signature.subRoot
    )

    val sumVk = (verifyKey.value, verifyKey.step)
    verify(prodSig, message.toArray, sumVk)
  }

  def update(privateKey: SecretKeyKesProduct, steps: Int): SecretKeyKesProduct = {
    val sk = updateKey(unpackSecret(privateKey), steps)

    SecretKeyKesProduct(
      sk._1,
      sk._2,
      sk._3,
      SignatureKesSum(
        sk._4._1,
        sk._4._2,
        sk._4._3
      ),
      privateKey.offset
    )
  }

  def getCurrentStep(privateKey: SecretKeyKesProduct): Int = getKeyTime(unpackSecret(privateKey))

  def getMaxStep(privateKey: SecretKeyKesProduct): Int = exp(
    getTreeHeight(privateKey.superTree) + getTreeHeight(privateKey.subTree)
  )

  def getVerificationKey(privateKey: SecretKeyKesProduct): VerificationKeyKesProduct = {
    val vk = generateVerificationKey(unpackSecret(privateKey))
    VerificationKeyKesProduct(vk._1, vk._2)
  }

  private def unpackSecret(privateKey: SecretKeyKesProduct): SK =
    (
      privateKey.superTree,
      privateKey.subTree,
      privateKey.nextSubSeed,
      (
        privateKey.subSignature.verificationKey,
        privateKey.subSignature.signature,
        privateKey.subSignature.witness.toVector
      )
    )
}

object KesProduct {
  val instance: KesProduct = new KesProduct
}

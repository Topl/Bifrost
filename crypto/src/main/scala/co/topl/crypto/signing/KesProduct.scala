package co.topl.crypto.signing

import co.topl.crypto.signing.kes.ProductComposition
import co.topl.models.Proofs.Knowledge
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Sized
import co.topl.models.{Bytes, Proofs, SecretKeys, VerificationKeys}

class KesProduct extends ProductComposition {

  def createKeyPair(
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
        sk._3,
        Proofs.Knowledge.KesSum(
          VerificationKeys.Ed25519(Sized.strictUnsafe(Bytes(sk._4._1))),
          Proofs.Knowledge.Ed25519(Sized.strictUnsafe(Bytes(sk._4._2))),
          sk._4._3.map(w => Sized.strictUnsafe[Bytes, Proofs.Knowledge.KesSum.DigestLength](Bytes(w)))
        ),
        offset
      ),
      VerificationKeys.KesProduct(Sized.strictUnsafe(Bytes(pk._1)), pk._2)
    )
  }

  def sign(privateKey: SecretKeys.KesProduct, message: Bytes): Knowledge.KesProduct = {
    val prodSig = sign(unpackSecret(privateKey), message.toArray)

    Proofs.Knowledge.KesProduct(
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519(Sized.strictUnsafe(Bytes(prodSig._1._1))),
        Proofs.Knowledge.Ed25519(Sized.strictUnsafe(Bytes(prodSig._1._2))),
        prodSig._1._3.map(w => Sized.strictUnsafe[Bytes, Proofs.Knowledge.KesSum.DigestLength](Bytes(w)))
      ),
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519(Sized.strictUnsafe(Bytes(prodSig._2._1))),
        Proofs.Knowledge.Ed25519(Sized.strictUnsafe(Bytes(prodSig._2._2))),
        prodSig._2._3.map(w => Sized.strictUnsafe[Bytes, Proofs.Knowledge.KesSum.DigestLength](Bytes(w)))
      ),
      Sized.strictUnsafe(Bytes(prodSig._3))
    )
  }

  def verify(
    signature: co.topl.consensus.models.SignatureKesProduct,
    message:   Bytes,
    verifyKey: co.topl.consensus.models.VerificationKeyKesProduct
  ): Boolean = {
    val prodSig = (
      (
        signature.getSuperSignature.getVerificationKey.toByteArray,
        signature.getSuperSignature.getSignature.value.toByteArray,
        signature.getSuperSignature.witness.toVector.map(_.toByteArray)
      ),
      (
        signature.getSubSignature.getVerificationKey.toByteArray,
        signature.getSubSignature.getSignature.value.toByteArray,
        signature.getSubSignature.witness.toVector.map(_.toByteArray)
      ),
      signature.subRoot.toByteArray
    )

    val sumVk = (verifyKey.value.toByteArray, verifyKey.step)
    verify(prodSig, message.toArray, sumVk)
  }

  def update(privateKey: SecretKeys.KesProduct, steps: Int): SecretKeys.KesProduct = {
    val sk = updateKey(unpackSecret(privateKey), steps)

    SecretKeys.KesProduct(
      sk._1,
      sk._2,
      sk._3,
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519(Sized.strictUnsafe(Bytes(sk._4._1))),
        Proofs.Knowledge.Ed25519(Sized.strictUnsafe(Bytes(sk._4._2))),
        sk._4._3.map(w => Sized.strictUnsafe[Bytes, Proofs.Knowledge.KesSum.DigestLength](Bytes(w)))
      ),
      privateKey.offset
    )
  }

  def getCurrentStep(privateKay: SecretKeys.KesProduct): Int = getKeyTime(unpackSecret(privateKay))

  def getMaxStep(privateKey: SecretKeys.KesProduct): Int = exp(
    getTreeHeight(privateKey.superTree) + getTreeHeight(privateKey.subTree)
  )

  def getVerificationKey(privateKey: SecretKeys.KesProduct): VerificationKeys.KesProduct = {
    val vk = generateVerificationKey(unpackSecret(privateKey))
    VerificationKeys.KesProduct(Sized.strictUnsafe(Bytes(vk._1)), vk._2)
  }

  private def unpackSecret(privateKey: SecretKeys.KesProduct): SK =
    (
      privateKey.superTree,
      privateKey.subTree,
      privateKey.nextSubSeed,
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

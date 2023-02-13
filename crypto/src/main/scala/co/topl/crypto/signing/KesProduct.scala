package co.topl.crypto.signing

import co.topl.crypto.signing.kes.ProductComposition
import co.topl.{models => legacyModels}
import legacyModels.Proofs.Knowledge
import legacyModels.utility.HasLength.instances._
import legacyModels.utility.Sized
import legacyModels.{Bytes, Proofs, SecretKeys, VerificationKeys}
import co.topl.consensus.models.{SignatureKesProduct, VerificationKeyKesProduct}

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

  /**
   * Throws: NoSuchElementException – if any signatures or vks are empty.
   */
  def verify(
    signature: SignatureKesProduct,
    message:   Bytes,
    verifyKey: VerificationKeyKesProduct
  ): Boolean = {
    val prodSig = (
      (
        signature.superSignature.verificationKey.value.toByteArray,
        signature.superSignature.signature.value.toByteArray,
        signature.superSignature.witness.map(_.toByteArray).toVector
      ),
      (
        signature.subSignature.verificationKey.value.toByteArray,
        signature.subSignature.signature.value.toByteArray,
        signature.subSignature.witness.map(_.toByteArray).toVector
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

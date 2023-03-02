package co.topl.crypto.signing

import co.topl.crypto.signing.kes.ProductComposition
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Sized
import co.topl.models.{Bytes, Proofs, SecretKeys, VerificationKeys}
import co.topl.crypto.models.{
  SignatureEd25519,
  SignatureKesProduct,
  SignatureKesSum,
  VerificationKeyEd25519,
  VerificationKeyKesProduct
}
import com.google.protobuf.ByteString

class KesProduct extends ProductComposition {

  def createKeyPair(
    seed:   Bytes,
    height: (Int, Int),
    offset: Long
  ): (SecretKeys.KesProduct, VerificationKeyKesProduct) = {
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
      VerificationKeyKesProduct(ByteString.copyFrom(pk._1), pk._2)
    )
  }

  def sign(privateKey: SecretKeys.KesProduct, message: Bytes): SignatureKesProduct = {
    val prodSig = sign(unpackSecret(privateKey), message.toArray)

    SignatureKesProduct(
      SignatureKesSum(
        VerificationKeyEd25519(ByteString.copyFrom(prodSig._1._1)),
        SignatureEd25519(ByteString.copyFrom(prodSig._1._2)),
        prodSig._1._3.map(w => ByteString.copyFrom(w))
      ),
      SignatureKesSum(
        VerificationKeyEd25519(ByteString.copyFrom(prodSig._2._1)),
        SignatureEd25519(ByteString.copyFrom(prodSig._2._2)),
        prodSig._2._3.map(w => ByteString.copyFrom(w))
      ),
      ByteString.copyFrom(prodSig._3)
    )
  }

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

  def getVerificationKey(privateKey: SecretKeys.KesProduct): VerificationKeyKesProduct = {
    val vk = generateVerificationKey(unpackSecret(privateKey))
    VerificationKeyKesProduct(ByteString.copyFrom(vk._1), vk._2)
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

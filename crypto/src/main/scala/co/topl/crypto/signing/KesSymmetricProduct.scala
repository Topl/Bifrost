package co.topl.crypto.signing

import co.topl.models.Proofs.Signature
import co.topl.models.{KeyData, SecretKeys, VerificationKeys}

class KesSymmetricProduct extends kes.KesEd25519Blake2b256 {

  def createKeyPair(seed: Seed, height: Int): (SecretKeys.KesSymmetricProduct, VerificationKeys.KesSymmetricProduct) = {
    val sk: KeyData = generateProductKeyData(seed.value, height)
    val pk: Array[Byte] = publicKey(sk)

    (SecretKeys.KesSymmetricProduct(sk), VerificationKeys.KesSymmetricProduct(pk))
  }

  def sign(privateKey: SecretKeys.KesSymmetricProduct, message: MessageToSign, index: Int): Signature.KesSum =
    Signature.KesSymmetricProduct(signSymmetricProduct(privateKey.tree, message.value, index))

  def verify(
    signature: Signature.KesSymmetricProduct,
    message:   MessageToSign,
    verifyKey: VerificationKeys.KesSymmetricProduct,
    index:     Int
  ): Boolean = sumCompositionVerify(verifyKey.bytes, message.value, signature.bytes, index: Int)

  def deriveSecret(secretKey: SecretKeys.KesSymmetricProduct, index: Int): SecretKeys.KesSymmetricProduct =
    SecretKeys.KesSymmetricProduct(sumCompositionUpdate(secretKey.tree, index))
}

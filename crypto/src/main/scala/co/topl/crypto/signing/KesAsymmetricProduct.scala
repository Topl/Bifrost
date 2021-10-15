package co.topl.crypto.signing

import co.topl.crypto.signing.kes.ProdAsymComp
import co.topl.models.Proofs.Signature
import co.topl.models.{KeyData, SecretKeys, VerificationKeys}

class KesAsymmetricProduct extends ProdAsymComp {

  def publicKey(key: SecretKeys.KesAsymmetricProduct): Array[Byte] =
    generateVerificationKey(key.data.superScheme)

  /**
   * Get the public key of an MMM private key
   * @param key input key
   * @return public key
   */
  def publicKey(key: KeyData): Array[Byte] =
    generateVerificationKey(key.superScheme)

  def createKeyPair(
    seed:   Seed,
    height: Int
  ): (SecretKeys.KesAsymmetricProduct, VerificationKeys.KesAsymmetricProduct) = {
    val sk: KeyData = generateAsymmetricProductKey(seed.value, height)
    val pk: Array[Byte] = publicKey(sk)

    (SecretKeys.KesAsymmetricProduct(sk), VerificationKeys.KesAsymmetricProduct(pk))
  }

  def sign(privateKey: SecretKeys.KesAsymmetricProduct, message: MessageToSign): Signature.KesAsymmetricProduct =
    Signature.KesAsymmetricProduct(signAsymmetricProduct(privateKey.data, message.value))

  def verify(
    signature: Signature.KesAsymmetricProduct,
    message:   MessageToSign,
    verifyKey: VerificationKeys.KesAsymmetricProduct,
    index:     Int
  ): Boolean = verify(verifyKey.bytes, message.value, signature.bytes, index: Int)

  //todo: fix
  //  def deriveSecret(secretKey: SecretKeys.KesAsymmetricProduct, index: Int): SecretKeys.KesAsymmetricProduct =
  //    SecretKeys.KesAsymmetricProduct(sumCompositionUpdate(secretKey.data.superScheme, index))

}

object KesAsymmetricProduct {
  val instance: KesAsymmetricProduct = new KesAsymmetricProduct
}

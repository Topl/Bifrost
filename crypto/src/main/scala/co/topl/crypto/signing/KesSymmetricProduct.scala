//package co.topl.crypto.signing
//
//import co.topl.crypto.signing.kes.ProdSymComp
//import co.topl.models.Proofs.Signature
//import co.topl.models.{KeyData, SecretKeys, VerificationKeys}
//
//class KesSymmetricProduct extends ProdSymComp {
//
//  def publicKey(key: SecretKeys.KesSymmetricProduct): Array[Byte] =
//    sumCompositionGetPublicKey(key.data.superScheme)
//
//  def createKeyPair(seed: Seed, height: Int): (SecretKeys.KesSymmetricProduct, VerificationKeys.KesSymmetricProduct) = {
//    val sk: KeyData = generateSymmetricProductKey(seed.value, height)
//    val pk: Array[Byte] = publicKey(sk)
//
//    (SecretKeys.KesSymmetricProduct(sk), VerificationKeys.KesSymmetricProduct(pk))
//  }
//
//  def sign(privateKey: SecretKeys.KesSymmetricProduct, message: MessageToSign): Signature.KesSymmetricProduct =
//    Signature.KesSymmetricProduct(signSymmetricProduct(privateKey.data, message.value))
//
//  def verify(
//    signature: Signature.KesSymmetricProduct,
//    message:   MessageToSign,
//    verifyKey: VerificationKeys.KesSymmetricProduct,
//    index:     Int
//  ): Boolean = sumCompositionVerify(verifyKey.bytes, message.value, signature.bytes, index: Int)
//
//  //todo: fix
////  def deriveSecret(secretKey: SecretKeys.KesSymmetricProduct, index: Int): SecretKeys.KesSymmetricProduct =
////    SecretKeys.KesSymmetricProduct(sumCompositionUpdate(secretKey.data.superScheme, index))
//}
//
//object KesSymmetricProduct {
//  val instance: KesSymmetricProduct = new KesSymmetricProduct
//}
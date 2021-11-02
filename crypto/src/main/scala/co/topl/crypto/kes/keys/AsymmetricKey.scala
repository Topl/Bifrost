//package co.topl.crypto.kes.keys
//
//import co.topl.crypto.kes.KeyEvolvingSignatureScheme
//import co.topl.models.{Bytes, KeyData, Proofs, SecretKeys}
//
///**
// * AMS 2021:
// * This is a private key of the MMM construction the asymmetric product composition with a specified time step offset
// * the offset is constrained by the public key and signatures include the offset,
// * The age of keys may be enforced in validation where the offset can be compared
// * to the time step of the signature since signatures include the offset
// */
//
//case class AsymmetricKey(override val data: KeyData) extends ProductPrivateKey {
//  import AsymmetricKey._
//
//  private def asModel: SecretKeys.AsymmetricMMM = SecretKeys.AsymmetricMMM(data)
//
//  def update(globalTimeStep: Long): AsymmetricKey =
//    AsymmetricKey(kes.updateAsymmetricProductKey(asModel, (globalTimeStep - data.offset).toInt).data)
//
//  def sign(message: Bytes): Proofs.Consensus.MMM =
//    kes.signAsymmetricProduct(asModel, message.toArray)
//
//  def getVerificationKey: PublicKey =
//    PublicKey(kes.publicKey(asModel))
//
//  def timeStepPlusOffset: Long =
//    kes.getAsymmetricProductKeyTimeStep(asModel) + data.offset
//
//  def timeStep: Long =
//    kes.getAsymmetricProductKeyTimeStep(asModel)
//
//  def getBytes: Bytes = ProductPrivateKey.serializer.getBytes(data)
//}
//
//object AsymmetricKey {
//
//
//
//}

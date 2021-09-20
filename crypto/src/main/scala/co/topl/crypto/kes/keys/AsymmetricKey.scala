package co.topl.crypto.kes.keys

import co.topl.crypto.kes.signatures.AsymmetricSignature
import co.topl.crypto.kes.KeyEvolvingSignatureScheme
import co.topl.crypto.kes.construction.KeyData
import co.topl.crypto.PublicKey

/**
 * AMS 2021:
 * This is a private key of the MMM construction the asymmetric product composition with a specified time step offset
 * the offset is constrained by the public key and signatures include the offset,
 * The age of keys may be enforced in validation where the offset can be compared
 * to the time step of the signature since signatures include the offset
 */

case class AsymmetricKey(override val data: KeyData) extends ProductPrivateKey {
  import AsymmetricKey._

  def update(globalTimeStep: Long): AsymmetricKey =
    kes.updateAsymmetricProductKey(this, (globalTimeStep - data.offset).toInt)

  def sign(message: Array[Byte]): AsymmetricSignature =
    kes.signAsymmetricProduct(this, message)

  def getVerificationKey: PublicKey =
    PublicKey(kes.publicKey(this))

  def timeStepPlusOffset: Long =
    kes.getAsymmetricProductKeyTimeStep(this) + data.offset

  def timeStep: Long =
    kes.getAsymmetricProductKeyTimeStep(this)

}

object AsymmetricKey {

  val kes: KeyEvolvingSignatureScheme = new KeyEvolvingSignatureScheme

  def newFromSeed(seed: Array[Byte], offset: Long): AsymmetricKey =
    kes.generateAsymmetricProductKey(seed, offset)

}

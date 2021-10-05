package co.topl.crypto.kes.keys

import co.topl.crypto.hash.blake2b256
import co.topl.crypto.kes.KeyEvolvingSignatureScheme
import co.topl.crypto.signing.Ed25519
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.Sized
import co.topl.models.{Bytes, KeyData, Proofs, SecretKeys}
import com.google.common.primitives.Longs

/**
 * AMS 2021:
 * This is a private key of the MMM construction the asymmetric product composition with a specified time step offset
 * the offset is constrained by the public key and signatures include the offset,
 * The age of keys may be enforced in validation where the offset can be compared
 * to the time step of the signature since signatures include the offset
 */

case class SymmetricKey(override val data: KeyData, signature: Proofs.Signature.Ed25519) extends ProductPrivateKey {

  import SymmetricKey._

  private def asModel: SecretKeys.SymmetricMMM = SecretKeys.SymmetricMMM(data, signature)

  def update(globalTimeStep: Long): SymmetricKey = {
    val m = kes.updateSymmetricProductKey(asModel, (globalTimeStep - data.offset).toInt)
    SymmetricKey(m.data, m.signature)
  }

  def sign(message: Bytes): Proofs.Consensus.MMM =
    ???

  def getVerificationKey: PublicKey =
    PublicKey(kes.publicKey(asModel))

  def timeStepPlusOffset: Long =
    kes.getSymmetricProductKeyTimeStep(asModel) + data.offset

  def timeStep: Long =
    kes.getSymmetricProductKeyTimeStep(asModel)

  override def getBytes: Bytes = signature.bytes.data ++ ProductPrivateKey.serializer.getBytes(data)

}

object SymmetricKey {

  val kes: KeyEvolvingSignatureScheme = new KeyEvolvingSignatureScheme

  val maxKeyTimeSteps: Int = kes.maxSymmetricKeyTimeSteps

  def newFromSeed(
    seed:   Array[Byte],
    offset: Long,
    signer: Bytes => Proofs.Signature.Ed25519
  ): SecretKeys.SymmetricMMM = {
    val kd = kes.generateProductKeyData(seed, offset)
    val m = blake2b256.hash(kes.publicKey(kd) ++ Longs.toByteArray(offset)).value
    val signature = signer(Bytes(m))
    SecretKeys.SymmetricMMM(kd, signature)
  }

  import ProductPrivateKey.serializer

  def deserializeSymmetricKey(bytes: Array[Byte]): SecretKeys.SymmetricMMM = {
    val byteStream = new ProductPrivateKey.ByteStream(Bytes(bytes), None)
    val numBytes = byteStream.getInt
    val sigBytes = byteStream.get(Ed25519.SignatureLength)
    serializer.fromBytes(
      new ProductPrivateKey.ByteStream(
        byteStream.get(numBytes - Ed25519.SignatureLength),
        ProductPrivateKey.DeserializeKey
      )
    ) match {
      case kd: KeyData =>
        SecretKeys.SymmetricMMM(kd, Proofs.Signature.Ed25519(Sized.strictUnsafe(sigBytes)))
    }
  }

}

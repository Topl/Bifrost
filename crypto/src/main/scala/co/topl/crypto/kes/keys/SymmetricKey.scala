package co.topl.crypto.kes.keys

import co.topl.crypto.PublicKey
import co.topl.crypto.hash.blake2b256
import co.topl.crypto.kes.KeyEvolvingSignatureScheme
import co.topl.crypto.kes.construction.KeyData
import co.topl.crypto.kes.signatures.SymmetricSignature
import co.topl.crypto.signatures.Ed25519
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.Sized
import co.topl.models.{Bytes, Proofs}
import com.google.common.primitives.Longs

/**
 * AMS 2021:
 * This is a private key of the MMM construction the asymmetric product composition with a specified time step offset
 * the offset is constrained by the public key and signatures include the offset,
 * The age of keys may be enforced in validation where the offset can be compared
 * to the time step of the signature since signatures include the offset
 */

case class SymmetricKey(override val data: KeyData, signature: Proofs.SignatureEd25519) extends ProductPrivateKey {

  import SymmetricKey._

  def update(globalTimeStep: Long): SymmetricKey =
    kes.updateSymmetricProductKey(this, (globalTimeStep - data.offset).toInt)

  def sign(message: Array[Byte]): SymmetricSignature =
    kes.signSymmetricProduct(this, message)

  def getVerificationKey: PublicKey =
    PublicKey(kes.publicKey(this))

  def timeStepPlusOffset: Long =
    kes.getSymmetricProductKeyTimeStep(this) + data.offset

  def timeStep: Long =
    kes.getSymmetricProductKeyTimeStep(this)

  override def getBytes: Array[Byte] = signature.bytes.data.toArray ++ ProductPrivateKey.serializer.getBytes(data)

}

object SymmetricKey {

  val kes: KeyEvolvingSignatureScheme = new KeyEvolvingSignatureScheme

  val maxKeyTimeSteps: Int = kes.maxSymmetricKeyTimeSteps

  def newFromSeed(seed: Array[Byte], offset: Long, signer: Bytes => Proofs.SignatureEd25519): SymmetricKey = {
    val kd = kes.generateProductKeyData(seed, offset)
    val m = blake2b256.hash(kes.publicKey(kd) ++ Longs.toByteArray(offset)).value
    val signature = signer(Bytes(m))
    SymmetricKey(kd, signature)
  }

  import ProductPrivateKey.serializer

  def deserializeSymmetricKey(bytes: Array[Byte]): SymmetricKey = {
    val byteStream = new ProductPrivateKey.ByteStream(bytes, None)
    val numBytes = byteStream.getInt
    val sigBytes = byteStream.get(Ed25519.SignatureLength)
    serializer.fromBytes(
      new ProductPrivateKey.ByteStream(
        byteStream.get(numBytes - Ed25519.SignatureLength),
        ProductPrivateKey.DeserializeKey
      )
    ) match {
      case kd: KeyData =>
        SymmetricKey(kd, Proofs.SignatureEd25519(Sized.strictUnsafe(Bytes(sigBytes))))
    }
  }

}

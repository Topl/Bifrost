package co.topl.crypto.kes

import co.topl.crypto.hash.blake2b256
import co.topl.crypto.kes.signatures.{AsymmetricSignature, ProductSignature, SumSignature, SymmetricSignature}
import co.topl.crypto.signatures.Ed25519
import com.google.common.primitives.Longs
import co.topl.crypto.signatures.Signature
import co.topl.crypto.PublicKey

object KesVerifier {

  val kes: KeyEvolvingSignatureScheme = new KeyEvolvingSignatureScheme
  val ec: Ed25519 = new Ed25519

  def verify(message: Array[Byte], sig: AsymmetricSignature, slot: Long): Boolean =
    kes.verifyProductSignature(message, sig: ProductSignature, (slot - sig.offset).toInt)

  def verify(message: Array[Byte], sig: SymmetricSignature, slot: Long): Boolean =
    kes.verifyProductSignature(message, sig: ProductSignature, (slot - sig.offset).toInt)

  def verify(message: Array[Byte], sig: SumSignature, slot: Long): Boolean =
    kes.sumCompositionVerify(sig.pkl.value, message, sig.bytes, (slot - sig.offset).toInt)

  def verify(vk_i: PublicKey, vk_kes: PublicKey, offset: Long, sig: Signature): Boolean = {
    val m = blake2b256.hash(vk_kes.value ++ Longs.toByteArray(offset)).value.array
    ec.verify(sig, m, vk_i)
  }

}

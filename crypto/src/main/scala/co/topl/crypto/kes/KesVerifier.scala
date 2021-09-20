package co.topl.crypto.kes

import co.topl.crypto.PublicKey
import co.topl.crypto.hash.blake2b256
import co.topl.crypto.kes.signatures.{AsymmetricSignature, ProductSignature, SumSignature, SymmetricSignature}
import co.topl.crypto.signatures.{Ed25519, Signature}
import co.topl.crypto.typeclasses.Signable
import co.topl.crypto.typeclasses.Signable.ops._
import co.topl.models.{Proofs, Slot}
import com.google.common.primitives.Longs

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

  def verify[Data: Signable](data: Data, proof: Proofs.Consensus.MMM, headerSlot: Slot): Boolean =
    kes.verifyProductSignature(
      data.signableBytes.toArray,
      SymmetricSignature(
        proof.sigi.toArray,
        proof.sigm.toArray,
        co.topl.crypto.PublicKey(proof.pki.toArray),
        proof.offset,
        co.topl.crypto.PublicKey(proof.pkl.toArray)
      ),
      (headerSlot - proof.offset).toInt
    )

}

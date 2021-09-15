package co.topl.crypto.kes

import co.topl.crypto.kes.signatures.{ProductSignature, SumSignature, SymmetricSignature}
import co.topl.crypto.typeclasses.Signable
import Signable.ops._
import co.topl.models.{KesCertificate, Proofs, Slot}

object KesVerifier {

  val kes: KeyEvolvingSignatureScheme = new KeyEvolvingSignatureScheme

  def verify(message: Array[Byte], sig: ProductSignature, timeStep: Int): Boolean =
    kes.verifyProductSignature(message, sig: ProductSignature, timeStep)

  def verify(message: Array[Byte], sig: SumSignature, timeStep: Int): Boolean =
    kes.sumCompositionVerify(sig.pkl.bytes, message, sig.bytes, timeStep)

  def verify[Data: Signable](data: Data, proof: Proofs.Consensus.MMM, headerSlot: Slot): Boolean =
    kes.verifyProductSignature(
      data.signableBytes.toArray,
      SymmetricSignature(
        proof.sigi.toArray,
        proof.sigm.toArray,
        co.topl.crypto.kes.keys.PublicKey(proof.pki.toArray),
        proof.offset,
        co.topl.crypto.kes.keys.PublicKey(proof.pkl.toArray)
      ),
      (headerSlot - proof.offset).toInt
    )

}

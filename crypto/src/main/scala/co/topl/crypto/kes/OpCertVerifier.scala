//package co.topl.crypto.kes
//
//import co.topl.crypto.signing.{Ed25519, KesSum}
//import co.topl.models.{Proofs, Slot}
//
//object OpCertVerifier {
//
//  val kes: KesSum = new KesSum
//  val ec: Ed25519 = new Ed25519
//
//  // todo: fix
//
////  def verify(message: Array[Byte], sig: AsymmetricSignature, slot: Long): Boolean =
////    kes.verifyProductSignature(message, sig: ProductSignature, (slot - sig.offset).toInt)
////
////  def verify(message: Array[Byte], sig: SymmetricSignature, slot: Long): Boolean =
////    kes.verifyProductSignature(message, sig: ProductSignature, (slot - sig.offset).toInt)
//
//  def verify(message: Array[Byte], sig: Proofs.Signature.KesSum, slot: Long): Boolean = true
//  //kes.sumCompositionVerify(sig.pkl.value, message, sig.bytes, (slot - sig.offset).toInt)
//
//  //def verify(vk_i: PublicKey, vk_kes: PublicKey, offset: Long, sig: Signature): Boolean = true
////  {
////    val m = blake2b256.hash(vk_kes.value ++ Longs.toByteArray(offset)).value.array
////    ec.verify(sig, m, vk_i)
////  }
//
//  def verify[Data: Signable](data: Data, proof: Proofs.Signature.HdKes, headerSlot: Slot): Boolean = true
////    kes.verifyProductSignature(
////      data.signableBytes.toArray,
////      SymmetricSignature(
////        proof.sigi.toArray,
////        proof.sigm.toArray,
////        co.topl.crypto.PublicKey(proof.pki.toArray),
////        proof.offset,
////        co.topl.crypto.PublicKey(proof.pkl.toArray)
////      ),
////      (headerSlot - proof.offset).toInt
////    )
//
//}

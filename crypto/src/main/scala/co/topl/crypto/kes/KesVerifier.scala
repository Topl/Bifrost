package co.topl.crypto.kes

import co.topl.crypto.kes.signatures.{ProductSignature, SumSignature}

object KesVerifier {

  val kes: KeyEvolvingSignatureScheme = new KeyEvolvingSignatureScheme

  def verify(message: Array[Byte], sig: ProductSignature, timeStep: Int): Boolean =
    kes.verifyProductSignature(message, sig: ProductSignature, timeStep)

  def verify(message: Array[Byte], sig: SumSignature, timeStep: Int): Boolean =
    kes.sumCompositionVerify(sig.pkl.bytes, message, sig.bytes, timeStep)

}

package co.topl.models

import co.topl.models.utility.{Lengths, Sized}

sealed trait Proof

object Proofs {

  object Signature {
    case class Curve25519(bytes: Sized.Strict[Bytes, Curve25519.Length]) extends Proof

    object Curve25519 {
      type Length = Lengths.`64`.type
    }

    case class Ed25519(bytes: Sized.Strict[Bytes, Ed25519.Length]) extends Proof

    object Ed25519 {
      type Length = Lengths.`64`.type
    }

    /**
     * @param ecSignature (elliptic curve)
     * @param vkK
     * @param index
     * @param witness
     */
    case class SumProduct(
      ecSignature: Signature.Ed25519,
      vkK:         VerificationKeys.Ed25519,
      index:       Long, // `k` or `j` depending on context
      witness:     Seq[VerificationKeys.Ed25519]
    ) extends Proof

    case class SymmetricProduct(
      sigSumJ: SumProduct,
      sigSumK: SumProduct
    ) extends Proof

    /**
     * @param i
     * @param vkI
     * @param ecSignature (elliptic curve) sign(L_i, sk_i)
     * @param sigSumJ _.ecSignature = sign(O_j, sk_j)
     * @param sigSumK _.ecSignature = sign(m, skK)
     */
    case class HdKes(
      i:           Long,
      vkI:         VerificationKeys.Ed25519,
      ecSignature: Signature.Ed25519,
      sigSumJ:     SumProduct,
      sigSumK:     SumProduct
    ) extends Proof

    case class VrfEd25519(bytes: Sized.Strict[Bytes, Lengths.`80`.type]) extends Proof
  }

  object Threshold {
    case class SignatureCurve25519(signatures: Set[Signature.Curve25519]) extends Proof
    case class SignatureEd25519(signatures: Set[Signature.Ed25519]) extends Proof
  }

  case class Existence(id: TypedIdentifier) extends Proof

//  object Consensus {
//
//    /**
//     * Signature with a witness path that corresponds to MMM construction
//     *
//     * @see [co.topl.crypto.kes.signatures.SymmetricSignature]
//     */
//    case class MMM(sigi: Bytes, sigm: Bytes, pki: Bytes, offset: Long, pkl: Bytes) extends Proof
//  }
}

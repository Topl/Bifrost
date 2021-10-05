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

    case class VrfEd25519(bytes: Sized.Strict[Bytes, VrfEd25519.Length]) extends Proof

    object VrfEd25519 {
      type Length = Lengths.`80`.type
    }

    case class KesMmmSum(
      ecSignature: Signature.Ed25519,
      vkK:         VerificationKeys.Ed25519,
      index:       Long, // `k` or `j` depending on context
      witness:     Seq[VerificationKeys.Ed25519]
    ) extends Proof

    object KesMmmSum {
      ///type Length = ???
    }

    case class KesMmmSymmetricProduct() extends Proof

    object KesMmmSymmetricProduct {
      ///type Length = ???
    }

    case class KesMmmAsymmetricProduct() extends Proof

    object KesMmmAsymmetricProduct {
      ///type Length = ???
    }

    case class HdKesMmmSum(
      i:           Long,
      vkI:         VerificationKeys.Ed25519,
      ecSignature: Signature.Ed25519,
      sigSumJ:     KesMmmSum,
      sigSumK:     KesMmmSum
    ) extends Proof

    object HdKesMmmSum {
      type Length = Lengths.`32`.type
    }

  }

  object Threshold {
    case class SignatureCurve25519(signatures: Set[Signature.Curve25519]) extends Proof
    case class SignatureEd25519(signatures: Set[Signature.Ed25519]) extends Proof
  }

  case class Existence(id: TypedIdentifier) extends Proof

  // TODO: Delete
  object Consensus {

    /**
     * Signature with a witness path that corresponds to MMM construction
     *
     * @see [co.topl.crypto.kes.signatures.SymmetricSignature]
     */
    case class MMM(sigi: Bytes, sigm: Bytes, pki: Bytes, offset: Long, pkl: Bytes) extends Proof
  }
}

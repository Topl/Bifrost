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

    case class KesSum(
      verificationKey: VerificationKeys.Ed25519,
      signature:       Proofs.Signature.Ed25519,
      witness:         Vector[Sized.Strict[Bytes, KesSum.DigestLength]]
    ) extends Proof

    object KesSum {
      type DigestLength = Lengths.`32`.type
    }

    case class KesProduct(
      superSignature: Proofs.Signature.KesSum,
      subSignature:   Proofs.Signature.KesSum,
      subRoot:        Sized.Strict[Bytes, KesProduct.DigestLength]
    ) extends Proof

    object KesProduct {
      type DigestLength = Lengths.`32`.type
    }
  }

  object Threshold {
    case class Curve25519(signatures: Set[Signature.Curve25519]) extends Proof
    case class Ed25519(signatures: Set[Signature.Ed25519]) extends Proof
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

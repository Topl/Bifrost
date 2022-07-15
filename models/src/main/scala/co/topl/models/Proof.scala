package co.topl.models

import co.topl.models.utility.{Lengths, Sized}

sealed trait Proof

object Proofs {

  /**
   * A proof which acts as a placeholder
   */
  case object Undefined extends Proof

  object Knowledge {
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
      signature:       Proofs.Knowledge.Ed25519,
      witness:         Vector[Sized.Strict[Bytes, KesSum.DigestLength]]
    ) extends Proof

    object KesSum {
      type DigestLength = Lengths.`32`.type
    }

    case class KesProduct(
      superSignature: Proofs.Knowledge.KesSum,
      subSignature:   Proofs.Knowledge.KesSum,
      subRoot:        Sized.Strict[Bytes, KesProduct.DigestLength]
    ) extends Proof

    object KesProduct {
      type DigestLength = Lengths.`32`.type
    }

    /**
     * This is not a zero-knowledge proof-of-knowledge
     */
    case class HashLock(value: Bytes) extends Proof
  }

  object Compositional {
    case class Threshold(proofs: List[Proof]) extends Proof
    case class And(a: Proof, b: Proof) extends Proof
    case class Or(a: Proof, b: Proof) extends Proof
    case class Not(a: Proof) extends Proof
  }

  object Contextual {
    case class HeightLock() extends Proof
    case class RequiredTransactionIO() extends Proof
  }
}

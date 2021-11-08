package co.topl.models

import scala.collection.immutable.SortedSet

sealed trait Proposition

object Propositions {

  object Knowledge {
    case class Curve25519(key: VerificationKeys.Curve25519) extends Proposition

    case class Ed25519(key: VerificationKeys.Ed25519) extends Proposition

    case class ExtendedEd25519(key: VerificationKeys.ExtendedEd25519) extends Proposition

    object Threshold {

      case class Curve25519(threshold: Int, propositions: SortedSet[VerificationKeys.Curve25519]) extends Proposition

      case class Ed25519(threshold: Int, propositions: SortedSet[VerificationKeys.Ed25519]) extends Proposition

      case class ExtendedEd25519(threshold: Int, propositions: SortedSet[VerificationKeys.ExtendedEd25519])
          extends Proposition
    }
  }

  // Note: Keep these propositions in mind for future development
  //  case class Existence(boxIdHash: Digest32) extends Proposition
  //  case class Timelock(height: Long) extends Proposition
  //  case class And(a: Proposition, b: Proposition) extends Proposition

  // TODO: Delete
  case class VerificationKeyVRF(key: VerificationKeys.VrfEd25519) extends Proposition

}

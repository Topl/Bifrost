package co.topl.models

import co.topl.models.utility.Lengths

import scala.collection.immutable.SortedSet

sealed trait Proposition

object Propositions {

  case class PublicKeyCurve25519(key: VerificationKeys.Curve25519) extends Proposition

  object PublicKeyCurve25519 {
    type Length = Lengths.`32`.type
  }

  case class ThresholdCurve25519(threshold: Int, propositions: SortedSet[VerificationKeys.Curve25519])
      extends Proposition

  case class PublicKeyEd25519(key: VerificationKeys.Ed25519) extends Proposition

  case class ThresholdEd25519(threshold: Int, propositions: SortedSet[VerificationKeys.Ed25519]) extends Proposition

  case class PublicKeyExtendedEd25519(key: VerificationKeys.ExtendedEd25519) extends Proposition

  case class Existence() extends Proposition

  case class VerificationKeyVRF(key: VerificationKeys.VrfEd25519)
      extends Proposition // I don't think these should be propositions (insofar as a proposition typically relates to transactions)

}

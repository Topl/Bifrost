package co.topl.models

import scala.collection.immutable.SortedSet

sealed trait Proposition

object Propositions {

  case class PublicKeyCurve25519(key: VerificationKeys.Curve25519) extends Proposition

  case class ThresholdCurve25519(threshold: Int, propositions: SortedSet[VerificationKeys.Curve25519])
      extends Proposition

  case class PublicKeyEd25519(key: VerificationKeys.Ed25519) extends Proposition

  case class ThresholdEd25519(threshold: Int, propositions: SortedSet[VerificationKeys.Ed25519]) extends Proposition

  case class PublicKeyExtendedEd25519(key: VerificationKeys.ExtendedEd25519) extends Proposition

  case class Existence() extends Proposition

  case class VerificationKeyVRF(key: VerificationKeys.Vrf) extends Proposition

  case class VerificationKeyHdKES(key: VerificationKeys.HdKes) extends Proposition

}

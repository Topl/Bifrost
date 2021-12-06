package co.topl.models

import scala.collection.immutable.ListSet

sealed trait Proposition

object Propositions {

  object Knowledge {
    case class Curve25519(key: VerificationKeys.Curve25519) extends Proposition
    case class Ed25519(key: VerificationKeys.Ed25519) extends Proposition
    case class ExtendedEd25519(key: VerificationKeys.ExtendedEd25519) extends Proposition
  }

  object Compositional {
    case class Threshold(threshold: Int, propositions: ListSet[Proposition]) extends Proposition
    case class And(a: Proposition, b: Proposition) extends Proposition
    case class Or(a: Proposition, b: Proposition) extends Proposition
  }

  object Contextual {
    case class HeightLock(height: Long) extends Proposition
  }

}

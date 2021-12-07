package co.topl.typeclasses

import co.topl.models.{Proposition, Propositions, VerificationKeys}

import scala.collection.immutable.ListSet
import scala.language.implicitConversions

/**
 * Reveal inputs to produce a Proposition
 * @tparam PropositionInput
 * @tparam Prop
 */
trait Proposer[PropositionInput, Prop <: Proposition] {

  /**
   * Creates a Proposition from the given T
   * @param t a value which can be converted into a Proposition (usually a Verification Key)
   * @return a Proposition
   */
  def propositionOf(t: PropositionInput): Prop
}

object Proposer {

  trait Implicits {

    implicit class AnyOps[T](t: T) {

      def proposition[Prop <: Proposition](implicit ev: Proposer[T, Prop]): Prop =
        ev.propositionOf(t)
    }

    implicit class CompositionalOps(p: Proposition) {
      def and(other: Proposition): Propositions.Compositional.And = Propositions.Compositional.And(p, other)
      def or(other:  Proposition): Propositions.Compositional.Or = Propositions.Compositional.Or(p, other)
    }

    implicit class IterableOps(props: Iterable[Proposition]) {

      def threshold(k: Int): Propositions.Compositional.Threshold =
        Propositions.Compositional.Threshold(k, ListSet.from(props))
    }
  }

  object implicits extends Implicits

  trait Instances {

    implicit val curve25519Proposes: Proposer[VerificationKeys.Curve25519, Propositions.Knowledge.Curve25519] =
      t => Propositions.Knowledge.Curve25519(t)

    implicit val ed25519Proposes: Proposer[VerificationKeys.Ed25519, Propositions.Knowledge.Ed25519] =
      t => Propositions.Knowledge.Ed25519(t)

    implicit val extendedEd25519Proposes
      : Proposer[VerificationKeys.ExtendedEd25519, Propositions.Knowledge.ExtendedEd25519] =
      t => Propositions.Knowledge.ExtendedEd25519(t)

    implicit val longProposesHeightLock: Proposer[Long, Propositions.Contextual.HeightLock] =
      t => Propositions.Contextual.HeightLock(t)
  }

  object instances extends Instances
}

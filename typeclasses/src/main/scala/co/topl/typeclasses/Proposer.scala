package co.topl.typeclasses

import co.topl.models.{Proposition, Propositions, VerificationKey, VerificationKeys}

import scala.collection.immutable.ListSet
import scala.language.implicitConversions

/**
 * Reveal inputs to produce a Proposition
 */
@simulacrum.typeclass
trait Proposer[PropositionInput] {

  /**
   * Creates a Proposition from the given T
   * @param t a value which can be converted into a Proposition (usually a Verification Key)
   * @return a Proposition
   */
  @simulacrum.op("asProposition")
  def propositionOf(t: PropositionInput): Proposition
}

object Proposer {

  trait Implicits {

    implicit class CompositionalOps(p: Proposition) {
      def and(other: Proposition): Propositions.Compositional.And = Propositions.Compositional.And(p, other)
      def or(other: Proposition): Propositions.Compositional.Or = Propositions.Compositional.Or(p, other)
    }

    implicit class IterableOps(props: Iterable[Proposition]) {

      def threshold(k: Int): Propositions.Compositional.Threshold =
        Propositions.Compositional.Threshold(k, ListSet.empty ++ props)
    }
  }

  object implicits extends Implicits

  trait Instances {

    implicit val vkProposes: Proposer[VerificationKey] = {
      case t: VerificationKeys.Curve25519      => Propositions.Knowledge.Curve25519(t)
      case t: VerificationKeys.Ed25519         => Propositions.Knowledge.Ed25519(t)
      case t: VerificationKeys.ExtendedEd25519 => Propositions.Knowledge.ExtendedEd25519(t)
      case t                                   => throw new MatchError(t)
    }

    implicit val curve25519Proposes: Proposer[VerificationKeys.Curve25519] =
      t => Propositions.Knowledge.Curve25519(t)

    implicit val ed25519Proposes: Proposer[VerificationKeys.Ed25519] =
      t => Propositions.Knowledge.Ed25519(t)

    implicit val extendedEd25519Proposes: Proposer[VerificationKeys.ExtendedEd25519] =
      t => Propositions.Knowledge.ExtendedEd25519(t)

    implicit val longProposesHeightLock: Proposer[Long] =
      t => Propositions.Contextual.HeightLock(t)

    implicit class StringOps(value: String) {
      def jsProposition: Propositions.Script.JS = Propositions.Script.JS(Propositions.Script.JS.JSScript(value))
    }
  }

  object instances extends Instances
}

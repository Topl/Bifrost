package co.topl.typeclasses

import co.topl.models.{Proposition, Propositions, VerificationKeys}

import scala.language.implicitConversions

trait Proposer[T, Prop <: Proposition] {

  /**
   * Creates a Proposition from the given T
   * @param t a value which can be converted into a Proposition (usually a Verification Key)
   * @return a Proposition
   */
  def propositionOf(t: T): Prop
}

object Proposer {

  trait Ops[T, Prop <: Proposition] {
    def t: T
    def typeclassInstance: Proposer[T, Prop]
    def proposition: Prop = typeclassInstance.propositionOf(t)
  }

  trait implicits {

    implicit def asProposesOps[T, Prop <: Proposition](t1: T)(implicit ev: Proposer[T, Prop]): Ops[T, Prop] =
      new Ops[T, Prop] {
        def t: T = t1

        def typeclassInstance: Proposer[T, Prop] = ev
      }
  }

  object implicits extends implicits

  trait Instances {

    implicit val curve25519Proposes: Proposer[VerificationKeys.Curve25519, Propositions.Knowledge.Curve25519] =
      t => Propositions.Knowledge.Curve25519(t)

    implicit val ed25519Proposes: Proposer[VerificationKeys.Ed25519, Propositions.Knowledge.Ed25519] =
      t => Propositions.Knowledge.Ed25519(t)

    implicit val extendedEd25519Proposes
      : Proposer[VerificationKeys.ExtendedEd25519, Propositions.Knowledge.ExtendedEd25519] =
      t => Propositions.Knowledge.ExtendedEd25519(t)

    implicit val vrfProposes: Proposer[VerificationKeys.VrfEd25519, Propositions.VerificationKeyVRF] =
      t => Propositions.VerificationKeyVRF(t)

    implicit def containsVerificationKeyProposes[T, VK, Prop <: Proposition](implicit
      containsVerificationKey: ContainsVerificationKey[T, VK],
      proposer:                Proposer[VK, Prop]
    ): Proposer[T, Prop] =
      t => proposer.propositionOf(containsVerificationKey.verificationKeyOf(t))
  }

  object instances extends Instances
}

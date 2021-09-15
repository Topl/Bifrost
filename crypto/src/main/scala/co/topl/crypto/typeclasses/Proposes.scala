package co.topl.crypto.typeclasses

import co.topl.models.{Proposition, Propositions, PublicKeys}

trait Proposes[T, Prop <: Proposition] {
  def propositionOf(t: T): Prop
}

object Proposes {

  trait Ops[T, Prop <: Proposition] {
    def t: T
    def typeclassInstance: Proposes[T, Prop]
    def proposition: Prop = typeclassInstance.propositionOf(t)
  }

  trait implicits {

    implicit def asProposesOps[T, Prop <: Proposition](t1: T)(implicit ev: Proposes[T, Prop]): Ops[T, Prop] =
      new Ops[T, Prop] {
        def t: T = t1

        def typeclassInstance: Proposes[T, Prop] = ev
      }
  }

  object implicits extends implicits

  trait Instances {

    implicit val ed25519Proposes: Proposes[PublicKeys.Ed25519, Propositions.PublicKeyEd25519] =
      t => Propositions.PublicKeyEd25519(t)

    implicit val vrfProposes: Proposes[PublicKeys.Vrf, Propositions.Consensus.PublicKeyVrf] =
      t => Propositions.Consensus.PublicKeyVrf(t)

    implicit val kesProposes: Proposes[PublicKeys.Kes, Propositions.Consensus.PublicKeyKes] =
      t => Propositions.Consensus.PublicKeyKes(t)
  }

  object instances extends Instances
}

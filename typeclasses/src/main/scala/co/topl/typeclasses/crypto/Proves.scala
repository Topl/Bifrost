package co.topl.typeclasses.crypto

import co.topl.crypto.signatures.Ed25519
import co.topl.models.Proofs.Consensus
import co.topl.models._
import co.topl.models.utility.HasLength.implicits._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.Sized
import co.topl.typeclasses.crypto.Signable.ops._

import scala.language.implicitConversions

trait Proposes[T, Prop <: Proposition] {
  def propositionOf(t: T): Prop
}

object Proposes {

  trait Ops[T, Prop <: Proposition] {
    def t: T
    def typeclassInstance: Proposes[T, Prop]
    def proposition: Prop = typeclassInstance.propositionOf(t)
  }

  trait Implicits {

    implicit def asProposesOps[T, Prop <: Proposition](t1: T)(implicit ev: Proposes[T, Prop]): Ops[T, Prop] =
      new Ops[T, Prop] {
        def t: T = t1

        def typeclassInstance: Proposes[T, Prop] = ev
      }
  }

  object Implicits extends Implicits

  trait Instances {

    implicit val ed25519Proposes: Proposes[PublicKeys.Ed25519, Propositions.PublicKeyEd25519] =
      t => Propositions.PublicKeyEd25519(t)

    implicit val kesProposes: Proposes[PublicKeys.Kes, Propositions.Consensus.PublicKeyKes] =
      t => Propositions.Consensus.PublicKeyKes(t)
  }
}

trait Proves[T, Prf <: Proof] {
  def proveWith[Data: Signable](t: T, data: Data): Prf
}

object Proves {

  trait Instances {

    implicit val kesPrivateKeyProves: Proves[PrivateKeys.Kes, Proofs.Consensus.KesCertificate] =
      new Proves[PrivateKeys.Kes, Proofs.Consensus.KesCertificate] {

        def proveWith[Data: Signable](t: PrivateKeys.Kes, data: Data): Consensus.KesCertificate =
          Consensus.KesCertificate(
            Sized
              .strict[Bytes, Proofs.Consensus.KesCertificate.Length](
                Bytes(Array.fill[Byte](64)(0))
//                Bytes(
//                  new Ed25519().sign(co.topl.crypto.PrivateKey(t.bytes.data.toArray), data.signableBytes.toArray).value
//                )
              )
              .toOption
              .get
          )
      }
  }
  object instances extends Instances
}

trait Verifiable[Prf <: Proof, Prop <: Proposition] {
  def verify[Data: Signable](proof: Prf, proposition: Prop, data: Data): Boolean
}

trait Evolves[F[_], T] {
  def evolve(t: T): F[T]
}

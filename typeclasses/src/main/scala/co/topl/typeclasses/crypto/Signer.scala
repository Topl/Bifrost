package co.topl.typeclasses.crypto

import co.topl.models.{Bytes, PublicKey}
import simulacrum.{op, typeclass}

trait Signer {
  def sign[Data: Signable](data: Data): Bytes
  def verificationKey: PublicKey
}

trait EvolvingSigner[F[_]] extends Signer {
  def update(): F[this.type]
}

@typeclass trait ContainsSigner[T] {
  @op("signer") def signerOf(t: T): Signer
}

trait ContainsEvolvingSigner[F[_], T] {
  def evolvingSignerOf(t: T): EvolvingSigner[F]
  def signerOf(t:         T): Signer = evolvingSignerOf(t)
}

object ContainsEvolvingSigner {

  trait Ops[F[_], T] {
    def value: T
    def typeclassInstance: ContainsEvolvingSigner[F, T]
    def evolvingSigner: EvolvingSigner[F] = typeclassInstance.evolvingSignerOf(value)
    def signer: Signer = typeclassInstance.signerOf(value)
  }

  trait Implicits {

    implicit def asOps[F[_], T](t: T)(implicit containsEvolvingSigner: ContainsEvolvingSigner[F, T]): Ops[F, T] =
      new Ops[F, T] {
        override def value: T = t

        override def typeclassInstance: ContainsEvolvingSigner[F, T] = containsEvolvingSigner
      }
  }
}

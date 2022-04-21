package co.topl.typeclasses

import cats.data.{NonEmptyChain, NonEmptyVector}
import simulacrum.typeclass

/**
 * Typeclass indicating that something can prepend elements
 */
@typeclass trait Prepend[F[_]] {
  def prepend[A](a: A, c: F[A]): F[A]
}

object Prepend {

  trait Instances {

    implicit val nonEmptyChainPrepend: Prepend[NonEmptyChain] =
      new Prepend[NonEmptyChain] {
        def prepend[A](a: A, c: NonEmptyChain[A]): NonEmptyChain[A] = c.prepend(a)
      }

    implicit val nonEmptyVectorPrepend: Prepend[NonEmptyVector] =
      new Prepend[NonEmptyVector] {
        def prepend[A](a: A, c: NonEmptyVector[A]): NonEmptyVector[A] = c.prepend(a)
      }
  }

  object instances extends Instances
}

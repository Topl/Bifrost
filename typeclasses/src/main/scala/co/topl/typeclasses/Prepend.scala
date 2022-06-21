package co.topl.typeclasses

import cats.data.{NonEmptyChain, NonEmptyVector}

/**
 * Typeclass indicating that something can prepend elements
 */
trait Prepend[F[_]] {
  def prepend[A](a: A, c: F[A]): F[A]
}

object Prepend {

  def apply[F[_]: Prepend]: Prepend[F] = implicitly

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

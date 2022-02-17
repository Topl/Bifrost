package co.topl.typeclasses

import cats.data.{NonEmptyChain, NonEmptyVector}
import simulacrum.typeclass

/**
 * Typeclass indicating something is non-empty
 */
@typeclass trait NonEmpty[F[_]] {
  def head[A](c: F[A]): A
}

object NonEmpty {

  trait Instances {

    implicit val nonEmptyChainNonEmpty: NonEmpty[NonEmptyChain] =
      new NonEmpty[NonEmptyChain] {
        def head[A](c: NonEmptyChain[A]): A = c.head
      }

    implicit val nonEmptyVectorNonEmpty: NonEmpty[NonEmptyVector] =
      new NonEmpty[NonEmptyVector] {
        def head[A](c: NonEmptyVector[A]): A = c.head
      }
  }

  object instances extends Instances
}

package co.topl.models.utility

import cats.implicits._
import cats.{Applicative, Functor}

/**
 * Represents a fallible morphism between two types, A and B.
 */
trait Morphism[F[_], A, B] {
  def aToB(fa: F[A]): F[Either[String, B]]
}

/**
 * Represents a fallible isomorphism between two types, A and B.
 */
case class Isomorphism[F[_], A, B](abMorphism: Morphism[F, A, B], baMorphism: Morphism[F, B, A])

object Isomorphism {

  def constant[F[_]: Functor, A, B](a: A, b: B): Isomorphism[F, A, B] =
    Isomorphism(_.as(b.asRight[String]), _.as(a.asRight[String]))

  trait Ops {

    implicit def isomorphismAsABMorphism[F[_], A, B](implicit
      isomorphism: Isomorphism[F, A, B]
    ): Morphism[F, A, B] =
      isomorphism.abMorphism

    implicit def isomorphismAsBAMorphism[F[_], A, B](implicit
      isomorphism: Isomorphism[F, A, B]
    ): Morphism[F, B, A] =
      isomorphism.baMorphism

    implicit class IsomorphicFValueOps[F[_], X](x: F[X]) {

      def to[B](implicit morphism: Morphism[F, X, B]): F[Either[String, B]] =
        morphism.aToB(x)
    }

    implicit class IsomorphicValueOps[X](x: X) {

      def toF[F[_]: Applicative, B](implicit morphism: Morphism[F, X, B]): F[Either[String, B]] =
        morphism.aToB(x.pure[F])
    }
  }

  object implicits extends Ops
}

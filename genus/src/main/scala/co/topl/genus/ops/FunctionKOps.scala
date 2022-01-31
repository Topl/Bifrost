package co.topl.genus.ops

import cats.~>

import scala.language.implicitConversions

final class FunctionKOps[F[_], A](val fa: F[A]) {
  def mapFunctor[G[_]](implicit fK: F ~> G): G[A] = fK.apply(fa)
}

object FunctionKOps {

  trait ToFunctionKOps {
    implicit def fromF[F[_], A](fa: F[A]): FunctionKOps[F, A] = new FunctionKOps[F, A](fa)
  }
}

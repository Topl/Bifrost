package co.topl.genus.ops

import cats.~>

import scala.language.implicitConversions

final class FunctionKOps[F[_], A](val fa: F[A]) {

  /**
   * Map from a value of functor F to a value of functor G.
   * @param fK the transformation function from F to G
   * @tparam G the resulting functor type
   * @return a value with functor G with the internal values unchanged
   */
  def mapFunctor[G[_]](implicit fK: F ~> G): G[A] = fK.apply(fa)
}

object FunctionKOps {

  trait ToFunctionKOps {
    implicit def fromF[F[_], A](fa: F[A]): FunctionKOps[F, A] = new FunctionKOps[F, A](fa)
  }
}

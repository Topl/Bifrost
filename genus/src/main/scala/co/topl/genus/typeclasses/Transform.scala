package co.topl.genus.typeclasses

import scala.language.implicitConversions

/**
 * Transforms a value from type A into type B
 * @tparam A the type to transform from
 * @tparam B the type to transform to
 */
trait Transform[A, B] {
  def transformTo(value: A): B
}

object Transform {

  def apply[A, B](implicit transform: Transform[A, B]): Transform[A, B] =
    transform

  /**
   * Extension operations on a value of type A.
   * @param value the underlying value to create operations for
   * @tparam A the type of the underlying value
   */
  class TransformOps[A](value: A) {

    /**
     * Transforms a value from type A to type B.
     * @param transform a Transform instance for types A and B
     * @tparam B the type to transform to
     * @return a value of type B
     */
    def transformTo[B](implicit transform: Transform[A, B]): B =
      transform.transformTo(value)
  }

  trait ToTransformOps {

    implicit def toTransformOps[A](value: A): TransformOps[A] =
      new TransformOps[A](value)
  }
}

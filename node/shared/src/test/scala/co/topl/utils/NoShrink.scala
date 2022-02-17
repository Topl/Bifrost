package co.topl.utils

import org.scalacheck.Shrink

trait NoShrink {

  /**
   * This def ensures that shrinks are disabled for all calls to forAll.
   *
   * If you want to enable shrinking for a specific test, introduce an
   * implicit val into that scope with type Shrink[T] where T is the type
   * of the generator you want to enable shrinking on.
   */
  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny[T]
}

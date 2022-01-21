package co.topl.algebras

/**
 * Wraps some entity of type T that is deemed to not be thread-safe.
 */
trait UnsafeResource[F[_], T] {

  /**
   * Use the thread-unsafe resource in a thread-safe manner
   */
  def use[Res](f: T => Res): F[Res]
}

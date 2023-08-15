package co.topl.algebras

import cats.effect.Sync

/**
 * Wraps some entity of type T that is deemed to not be thread-safe.
 */
trait UnsafeResource[F[_], T] {

  /**
   * Use the thread-unsafe resource in a thread-safe manner
   */
  def use[Res](f: T => F[Res]): F[Res]

  /**
   * Convenience method to execute the function using cats-effect Sync
   */
  def useSync[Res](f: T => Res)(implicit SyncF: Sync[F]): F[Res] = use(v => SyncF.delay(f(v)))
}

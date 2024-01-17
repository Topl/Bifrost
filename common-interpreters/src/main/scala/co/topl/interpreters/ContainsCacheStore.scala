package co.topl.interpreters

import cats.effect.kernel.Sync
import co.topl.algebras.Store
import com.github.benmanes.caffeine.cache.Caffeine
import scalacache.Entry
import scalacache.caffeine.CaffeineCache
import cats.implicits._

object ContainsCacheStore {

  /**
   * Wrap existing store with added cache for "contains" operation
   * @tparam F parameter type
   * @tparam Key the underlying Store Key type
   * @tparam Value the underlying Store Value type
   * @return a new Store which wraps the underlying Store with Caffeine Cache for contains operations
   */
  def make[F[_]: Sync, Key, Value](
    underlying:        F[Store[F, Key, Value]],
    containsCacheSize: Long
  ): F[Store[F, Key, Value]] =
    underlying.flatMap(underlying =>
      Sync[F]
        .delay[CaffeineCache[F, Key, Boolean]](
          CaffeineCache[F, Key, Boolean](
            Caffeine.newBuilder.maximumSize(containsCacheSize).build[Key, Entry[Boolean]]
          )
        )
        .map(containsCache =>
          new Store[F, Key, Value] {

            def put(id: Key, t: Value): F[Unit] =
              underlying.put(id, t) >> containsCache.put(id)(true)

            def remove(id: Key): F[Unit] =
              containsCache.put(id)(false) >> underlying.remove(id)

            def get(id: Key): F[Option[Value]] = underlying.get(id)

            def contains(id: Key): F[Boolean] =
              containsCache.cachingF(id)(ttl = None)(Sync[F].defer(underlying.contains(id)))
          }
        )
    )

  implicit class ContainsCacheStoreSyntax[F[_]: Sync, Key, Value](store: Store[F, Key, Value]) {
    def withCachedContains(containsCacheSize: Long): F[Store[F, Key, Value]] = make(store.pure[F], containsCacheSize)
  }
}

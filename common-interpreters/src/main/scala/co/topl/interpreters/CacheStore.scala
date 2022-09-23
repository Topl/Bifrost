package co.topl.interpreters

import cats.data.OptionT
import cats.implicits._
import cats.effect.kernel.Sync
import co.topl.algebras.Store
import com.github.benmanes.caffeine.cache.Caffeine
import scalacache.Entry
import scalacache.caffeine.CaffeineCache

import scala.concurrent.duration.FiniteDuration

object CacheStore {

  /**
   * Wraps an existing Store with a caching layer
   * @param underlying The underlying Store to wrap
   * @param makeKey a function which converts the underlying Store's Key type to a CacheKey type
   * @param cacheSettings a function which modifies the CaffeineCache builder with various settings
   * @param ttl the default TTL for new entries in the cache
   * @tparam Key the underlying Store Key type
   * @tparam CacheKey A separate Key type used by the caching layer.
   *                  The cache requires that the Keys extend Object, meaning primitives and newtypes are not allowed.
   * @tparam Value the underlying Store Value type
   * @return a new Store which wraps the underlying Store with Caffeine Cache
   */
  def make[F[_]: Sync, Key, CacheKey <: AnyRef, Value](
    underlying:    F[Store[F, Key, Value]],
    makeKey:       Key => CacheKey,
    cacheSettings: Caffeine[Object, Object] => Caffeine[Object, Object] = identity,
    ttl:           Option[FiniteDuration] = None
  ): F[Store[F, Key, Value]] =
    underlying.flatMap(underlying =>
      Sync[F]
        .delay[CaffeineCache[F, CacheKey, Option[Value]]](
          CaffeineCache[F, CacheKey, Option[Value]](
            cacheSettings(Caffeine.newBuilder).build[CacheKey, Entry[Option[Value]]]
          )
        )
        .map(implicit cache =>
          new Store[F, Key, Value] {

            def put(id: Key, t: Value): F[Unit] =
              (cache.put(makeKey(id))(t.some, ttl), underlying.put(id, t)).tupled.void

            def remove(id: Key): F[Unit] =
              (cache.put(makeKey(id))(None, ttl), underlying.remove(id)).tupled.void

            def get(id: Key): F[Option[Value]] =
              cache.cachingF(makeKey(id))(ttl = ttl)(
                Sync[F].defer(
                  underlying.get(id)
                )
              )

            def contains(id: Key): F[Boolean] =
              OptionT(cache.get(makeKey(id)))
                .foldF(underlying.contains(id))(_.nonEmpty.pure[F])
          }
        )
    )
}

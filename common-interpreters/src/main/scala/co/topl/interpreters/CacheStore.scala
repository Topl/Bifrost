package co.topl.interpreters

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
        .delay[CaffeineCache[F, CacheKey, Value]](
          CaffeineCache[F, CacheKey, Value](
            cacheSettings(Caffeine.newBuilder).build[CacheKey, Entry[Value]]
          )
        )
        .map(implicit cache =>
          new Store[F, Key, Value] {

            def put(id: Key, t: Value): F[Unit] =
              underlying.put(id, t) >>
              underlying.get(id).flatMap {
                case Some(d) => cache.put(makeKey(id))(d, ttl)
                case None    => ().pure[F]
              }

            def remove(id: Key): F[Unit] =
              (cache.remove(makeKey(id)), underlying.remove(id)).tupled.void

            def get(id: Key): F[Option[Value]] = {
              val key = makeKey(id)
              cache.get(key).flatMap {
                case Some(valueFromCache) => valueFromCache.some.pure[F]
                case None =>
                  Sync[F].defer(underlying.get(id)).flatTap {
                    case Some(valueFromUnder) => cache.put(key)(valueFromUnder, ttl)
                    case None                 => ().pure[F]
                  }
              }
            }

            def contains(id: Key): F[Boolean] =
              cache.get(makeKey(id)).flatMap {
                case Some(_) => true.pure[F]
                case None    => underlying.contains(id)
              }
          }
        )
    )
}

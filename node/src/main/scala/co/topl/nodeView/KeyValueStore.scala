package co.topl.nodeView

import co.topl.db.LDBVersionedStore
import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache}

import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}

trait KeyValueStore extends AutoCloseable {

  def update(
    version:  Array[Byte],
    toRemove: Iterable[Array[Byte]],
    toAdd:    Iterable[(Array[Byte], Array[Byte])]
  ): Unit
  def rollbackTo(version: Array[Byte]): Unit
  def get(key:            Array[Byte]): Option[Array[Byte]]
  def latestVersionId(): Option[Array[Byte]]
}

class LDBKeyValueStore(ldbStore: LDBVersionedStore) extends KeyValueStore {

  override def update(
    version:  Array[Byte],
    toRemove: Iterable[Array[Byte]],
    toAdd:    Iterable[(Array[Byte], Array[Byte])]
  ): Unit =
    ldbStore.update(version, toRemove, toAdd)

  override def rollbackTo(version: Array[Byte]): Unit =
    ldbStore.rollbackTo(version)

  override def get(key: Array[Byte]): Option[Array[Byte]] =
    ldbStore.get(key)

  override def close(): Unit =
    ldbStore.close()

  override def latestVersionId(): Option[Array[Byte]] =
    ldbStore.lastVersionID()
}

class CacheLayerKeyValueStore(underlying: KeyValueStore, cacheExpiration: FiniteDuration, cacheSize: Int)
    extends KeyValueStore {

  private[nodeView] val cache: LoadingCache[CacheLayerKeyValueStore.WrappedBytes, Option[Array[Byte]]] = CacheBuilder
    .newBuilder()
    .expireAfterAccess(cacheExpiration.toMillis, MILLISECONDS)
    .maximumSize(cacheSize)
    .build[CacheLayerKeyValueStore.WrappedBytes, Option[Array[Byte]]](
      new CacheLoader[CacheLayerKeyValueStore.WrappedBytes, Option[Array[Byte]]] {

        def load(key: CacheLayerKeyValueStore.WrappedBytes): Option[Array[Byte]] =
          underlying.get(key.bytes)
      }
    )

  override def update(
    version:  Array[Byte],
    toRemove: Iterable[Array[Byte]],
    toAdd:    Iterable[(Array[Byte], Array[Byte])]
  ): Unit = {
    underlying.update(version, toRemove, toAdd)
    toRemove.foreach(cache.invalidate)
    toAdd
      .map { case (key, value) => new CacheLayerKeyValueStore.WrappedBytes(key) -> Some(value) }
      .foreach((cache.put _).tupled)
  }

  override def rollbackTo(version: Array[Byte]): Unit = {
    underlying.rollbackTo(version)
    cache.invalidateAll()
  }

  override def get(key: Array[Byte]): Option[Array[Byte]] =
    cache.get(new CacheLayerKeyValueStore.WrappedBytes(key))

  override def close(): Unit =
    underlying.close()

  override def latestVersionId(): Option[Array[Byte]] =
    underlying.latestVersionId()
}

object CacheLayerKeyValueStore {

  private[nodeView] class WrappedBytes(val bytes: Array[Byte]) {

    override def equals(obj: Any): Boolean = obj match {
      case o: WrappedBytes => java.util.Arrays.equals(bytes, o.bytes)
      case _               => false
    }

    override def hashCode(): Int = java.util.Arrays.hashCode(bytes)
  }

}

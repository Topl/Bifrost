package co.topl.storage.leveldb

import akka.actor.typed.{ActorSystem, DispatcherSelector}
import co.topl.storage.generic.SetStore
import com.github.benmanes.caffeine.cache.Caffeine
import scalacache._
import scalacache.caffeine._
import scalacache.modes.sync._

import java.nio.file.{Path, Paths}
import scala.concurrent.ExecutionContext

class LevelDBStore(path: Path)(implicit system: ActorSystem[_]) extends AutoCloseable {

  implicit private val ec: ExecutionContext =
    system.dispatchers.lookup(DispatcherSelector.blocking())

  implicit private val setCache: Cache[LevelDBSetStore[_]] =
    CaffeineCache(
      Caffeine.newBuilder
        .executor(system.executionContext)
        .removalListener((_: String, v: Entry[LevelDBSetStore[_]], _) => v.value.close())
        .build[String, Entry[LevelDBSetStore[_]]]
    )

  def forSet[T: LevelDBStore.BytesCodec](name: String): SetStore[T] =
    setCache
      .caching(name)(ttl = None)(LevelDBSetStore[T](Paths.get(path.toString, name)))
      .asInstanceOf[SetStore[T]]

  override def close(): Unit = {
    setCache.removeAll()
    setCache.close()
  }

}

object LevelDBStore {

  trait BytesCodec[V] {
    def asBytes(v:       V): Array[Byte]
    def fromBytes(bytes: Array[Byte]): V
  }
}

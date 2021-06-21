package co.topl.storage.leveldb

import akka.actor.typed.{ActorSystem, DispatcherSelector}
import co.topl.storage.generic.{MapStore, SetStore}

import java.nio.file.{Path, Paths}
import scala.collection.concurrent.TrieMap
import scala.concurrent.ExecutionContext

class LevelDBStore(path: Path)(implicit system: ActorSystem[_]) extends AutoCloseable {

  implicit private val ec: ExecutionContext =
    system.dispatchers.lookup(DispatcherSelector.blocking())

  private val setStores: TrieMap[String, LevelDBSetStore[_]] =
    TrieMap.empty

  private val mapStores: TrieMap[String, LevelDBMapStore[_, _]] =
    TrieMap.empty

  def forSet[T: LevelDBStore.BytesCodec](name: String): SetStore[T] =
    setStores
      .getOrElseUpdate(name, LevelDBSetStore[T](Paths.get(path.toString, name)))
      .asInstanceOf[SetStore[T]]

  def forMap[K: LevelDBStore.BytesCodec, V: LevelDBStore.BytesCodec](name: String): MapStore[K, V] =
    mapStores
      .getOrElseUpdate(
        name,
        LevelDBMapStore[K, V](Paths.get(path.toString, name))
      )
      .asInstanceOf[MapStore[K, V]]

  override def close(): Unit = {
    setStores.keys.foreach(setStores.remove(_).foreach(_.close()))
    mapStores.keys.foreach(mapStores.remove(_).foreach(_.close()))
  }

}

object LevelDBStore {

  trait BytesCodec[V] {
    def asBytes(v:       V): Array[Byte]
    def fromBytes(bytes: Array[Byte]): V
  }
}

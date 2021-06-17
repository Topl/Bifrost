package co.topl.storage.mapdb

import akka.actor.typed.{ActorSystem, DispatcherSelector}
import akka.stream.ActorAttributes
import akka.stream.scaladsl.{Sink, Source}
import akka.{Done, NotUsed}
import cats.data.EitherT
import org.mapdb._
import org.mapdb.serializer.GroupSerializer

import java.nio.file.Path
import java.util
import scala.collection.concurrent.TrieMap
import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.CollectionConverters._

class MapDBStore(db: DB)(implicit system: ActorSystem[_]) extends AutoCloseable {

  implicit private val ec: ExecutionContext =
    system.dispatchers.lookup(DispatcherSelector.blocking())

  private val setStores: TrieMap[String, SetStore[_]] =
    TrieMap.empty

  private val mapStores: TrieMap[String, MapStore[_, _]] =
    TrieMap.empty

  def forSet[T: GroupSerializer](name: String): SetStore[T] =
    setStores
      .getOrElseUpdate(name, new NavigableSetStore[T](db.treeSet(name).serializer[T](implicitly).createOrOpen()))
      .asInstanceOf[SetStore[T]]

  def forMap[K: Serializer, V: Serializer](name: String): MapStore[K, V] =
    mapStores
      .getOrElseUpdate(
        name,
        new HTreeMapStore[K, V](
          db.hashMap(name)
            .keySerializer(implicitly[Serializer[K]])
            .valueSerializer(implicitly[Serializer[V]])
            .createOrOpen()
        )
      )
      .asInstanceOf[MapStore[K, V]]

  override def close(): Unit = db.close()
}

private class NavigableSetStore[T](treeSet: util.NavigableSet[T])(implicit blockingEc: ExecutionContext)
    extends SetStore[T] {

  override def contains(t: T): EitherT[Future, MapDBStore.Error, Boolean] =
    f(treeSet.contains(t))

  override def put(t: T): EitherT[Future, MapDBStore.Error, Boolean] =
    f(treeSet.add(t))

  override def remove(t: T): EitherT[Future, MapDBStore.Error, Boolean] =
    f(treeSet.remove(t))

  private def f[R](r: => R): EitherT[Future, MapDBStore.Error, R] =
    EitherT(Future(r).map(Right(_)).recover { case e => Left(MapDBStore.ThrowableError(e)) })

  override def values(): Source[T, NotUsed] =
    Source
      .fromIterator(() => treeSet.iterator().asScala)
      .addAttributes(ActorAttributes.dispatcher(ActorAttributes.IODispatcher.dispatcher))

  override def putMany(): Sink[T, Future[Done]] =
    Sink
      .foreach(treeSet.add)
      .addAttributes(ActorAttributes.dispatcher(ActorAttributes.IODispatcher.dispatcher))
}

private class HTreeMapStore[K, V](store: HTreeMap[K, V])(implicit blockingEc: ExecutionContext) extends MapStore[K, V] {

  override def contains(k: K): EitherT[Future, MapDBStore.Error, Boolean] =
    f(store.containsKey(store))

  override def put(k: K, v: V): EitherT[Future, MapDBStore.Error, Boolean] =
    f(store.putIfAbsentBoolean(k, v))

  override def remove(k: K): EitherT[Future, MapDBStore.Error, Boolean] =
    f(Option(store.remove(k)).nonEmpty)

  override def keys(): Source[K, NotUsed] =
    Source
      .fromIterator(() => store.getKeys.iterator().asScala)
      .addAttributes(ActorAttributes.dispatcher(ActorAttributes.IODispatcher.dispatcher))

  override def values(): Source[V, NotUsed] =
    Source
      .fromIterator(() => store.getValues.iterator().asScala)
      .addAttributes(ActorAttributes.dispatcher(ActorAttributes.IODispatcher.dispatcher))

  override def keyValues(): Source[(K, V), NotUsed] =
    Source
      .fromIterator(() => store.getEntries.iterator().asScala.map(entry => (entry.getKey, entry.getValue)))
      .addAttributes(ActorAttributes.dispatcher(ActorAttributes.IODispatcher.dispatcher))

  override def putMany(): Sink[(K, V), Future[Done]] =
    Sink
      .foreach[(K, V)] { case (k, v) => store.putIfAbsent(k, v) }
      .addAttributes(ActorAttributes.dispatcher(ActorAttributes.IODispatcher.dispatcher))

  private def f[R](r: => R): EitherT[Future, MapDBStore.Error, R] =
    EitherT(Future(r).map(Right(_)).recover { case e => Left(MapDBStore.ThrowableError(e)) })

  override def get(k: K): EitherT[Future, MapDBStore.Error, Option[V]] =
    f(Option(store.get(k)))

}

object MapDBStore {

  def disk(file: Path)(implicit system: ActorSystem[_]): MapDBStore =
    new MapDBStore(DBMaker.fileDB(file.toFile).make())

  def memory()(implicit system: ActorSystem[_]): MapDBStore =
    new MapDBStore(DBMaker.heapDB().make())

  sealed trait Error
  case class ThrowableError(throwable: Throwable) extends Error

  trait Implicits {
    implicit val stringSerializer: GroupSerializer[String] = Serializer.STRING
  }

  object implicits extends Implicits
}

trait SetStore[T] {
  def contains(t: T): EitherT[Future, MapDBStore.Error, Boolean]
  def put(t:      T): EitherT[Future, MapDBStore.Error, Boolean]
  def remove(t:   T): EitherT[Future, MapDBStore.Error, Boolean]
  def values(): Source[T, NotUsed]
  def putMany(): Sink[T, Future[Done]]
}

trait MapStore[K, V] {
  def contains(k: K): EitherT[Future, MapDBStore.Error, Boolean]
  def get(k:      K): EitherT[Future, MapDBStore.Error, Option[V]]
  def put(k:      K, v: V): EitherT[Future, MapDBStore.Error, Boolean]
  def remove(k:   K): EitherT[Future, MapDBStore.Error, Boolean]
  def keys(): Source[K, NotUsed]
  def values(): Source[V, NotUsed]
  def keyValues(): Source[(K, V), NotUsed]
  def putMany(): Sink[(K, V), Future[Done]]
}

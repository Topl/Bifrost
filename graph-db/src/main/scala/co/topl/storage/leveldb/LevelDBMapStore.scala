package co.topl.storage.leveldb

import akka.actor.typed.{ActorSystem, DispatcherSelector}
import akka.stream.ActorAttributes
import akka.stream.scaladsl.{Flow, Keep, Sink, Source}
import akka.{Done, NotUsed}
import cats.data.EitherT
import co.topl.storage.generic.{GenericStore, MapStore}
import org.iq80.leveldb.{DB, Options}

import java.nio.file.{Files, Path}
import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.CollectionConverters._

class LevelDBMapStore[K, V](db: DB)(implicit
  system:                       ActorSystem[_],
  keyCodec:                     LevelDBStore.BytesCodec[K],
  valueCodec:                   LevelDBStore.BytesCodec[V]
) extends MapStore[K, V] {

  implicit private val ec: ExecutionContext =
    system.dispatchers.lookup(DispatcherSelector.blocking())

  override def contains(k: K): EitherT[Future, GenericStore.Error, Boolean] =
    f(Option(db.get(keyCodec.asBytes(k))).nonEmpty)

  override def get(k: K): EitherT[Future, GenericStore.Error, Option[V]] =
    f(Option(db.get(keyCodec.asBytes(k))).map(valueCodec.fromBytes))

  override def put(k: K, v: V): EitherT[Future, GenericStore.Error, Boolean] =
    f {
      db.put(keyCodec.asBytes(k), valueCodec.asBytes(v))
      true
    }

  override def remove(k: K): EitherT[Future, GenericStore.Error, Boolean] =
    f {
      db.delete(keyCodec.asBytes(k))
      true
    }

  override def keys(): Source[K, NotUsed] =
    Source
      .fromIterator { () =>
        val it =
          db.iterator()
        it.seekToFirst()
        it.asScala.map(_.getKey).map(keyCodec.fromBytes)
      }
      .addAttributes(ActorAttributes.dispatcher(ActorAttributes.IODispatcher.dispatcher))

  override def values(): Source[V, NotUsed] =
    Source
      .fromIterator { () =>
        val it =
          db.iterator()
        it.seekToFirst()
        it.asScala.map(_.getValue).map(valueCodec.fromBytes)
      }
      .addAttributes(ActorAttributes.dispatcher(ActorAttributes.IODispatcher.dispatcher))

  override def keyValues(): Source[(K, V), NotUsed] =
    Source
      .fromIterator { () =>
        val it =
          db.iterator()
        it.seekToFirst()
        it.asScala.map(entry => keyCodec.fromBytes(entry.getKey) -> valueCodec.fromBytes(entry.getValue))
      }
      .addAttributes(ActorAttributes.dispatcher(ActorAttributes.IODispatcher.dispatcher))

  override def putMany(): Sink[(K, V), Future[Done]] =
    Flow[(K, V)]
      .grouped(100)
      .toMat(
        Sink
          .foreach[Seq[(K, V)]] { vs =>
            val batch = db.createWriteBatch()
            vs.foreach { case (k, v) => db.put(keyCodec.asBytes(k), valueCodec.asBytes(v)) }
            db.write(batch)
            batch.close()
          }
          .addAttributes(ActorAttributes.dispatcher(ActorAttributes.IODispatcher.dispatcher))
      )(Keep.right)

  private def f[R](r: => R): EitherT[Future, GenericStore.Error, R] =
    EitherT(Future(r).map(Right(_)).recover { case e => Left(GenericStore.ThrowableError(e)) })

  def close(): Unit =
    db.close()
}

object LevelDBMapStore {
  import org.fusesource.leveldbjni.JniDBFactory._

  def apply[K: LevelDBStore.BytesCodec, V: LevelDBStore.BytesCodec](
    path:            Path
  )(implicit system: ActorSystem[_]): LevelDBMapStore[K, V] = {
    Files.createDirectories(path)
    val options = new Options()
    options.createIfMissing(true)
    val db = factory.open(path.toFile, options)
    new LevelDBMapStore[K, V](db)
  }
}

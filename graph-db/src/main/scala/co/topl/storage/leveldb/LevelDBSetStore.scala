package co.topl.storage.leveldb

import akka.actor.typed.{ActorSystem, DispatcherSelector}
import akka.stream.ActorAttributes
import akka.stream.scaladsl.{Flow, Keep, Sink, Source}
import akka.{Done, NotUsed}
import cats.data.EitherT
import co.topl.storage.generic.{GenericStore, SetStore}
import org.iq80.leveldb._

import java.nio.file.{Files, Path}
import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.CollectionConverters._

class LevelDBSetStore[V](db: DB)(implicit system: ActorSystem[_], codec: LevelDBStore.BytesCodec[V])
    extends SetStore[V] {

  implicit private val ec: ExecutionContext =
    system.dispatchers.lookup(DispatcherSelector.blocking())

  override def contains(t: V): EitherT[Future, GenericStore.Error, Boolean] =
    f(Option(db.get(codec.asBytes(t))).nonEmpty)

  override def put(t: V): EitherT[Future, GenericStore.Error, Boolean] =
    f {
      db.put(codec.asBytes(t), LevelDBSetStore.ValueArray)
      true
    }

  override def remove(t: V): EitherT[Future, GenericStore.Error, Boolean] =
    f {
      db.delete(codec.asBytes(t))
      true
    }

  override def values(): Source[V, NotUsed] =
    Source
      .fromIterator { () =>
        val it =
          db.iterator()
        it.seekToFirst()
        it.asScala.map(_.getKey).map(codec.fromBytes)
      }
      .addAttributes(ActorAttributes.dispatcher(ActorAttributes.IODispatcher.dispatcher))

  override def putMany(): Sink[V, Future[Done]] =
    Flow[V]
      .grouped(100)
      .toMat(
        Sink
          .foreach[Seq[V]] { vs =>
            val batch = db.createWriteBatch()
            vs.foreach(v => batch.put(codec.asBytes(v), LevelDBSetStore.ValueArray))
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

object LevelDBSetStore {
  import org.fusesource.leveldbjni.JniDBFactory._

  def apply[K: LevelDBStore.BytesCodec](
    path:            Path
  )(implicit system: ActorSystem[_]): LevelDBSetStore[K] = {
    Files.createDirectories(path)
    val options = new Options()
    options.createIfMissing(true)
    val db = factory.open(path.toFile, options)
    new LevelDBSetStore[K](db)
  }

  final private[leveldb] val ValueArray: Array[Byte] = Array.emptyByteArray
}

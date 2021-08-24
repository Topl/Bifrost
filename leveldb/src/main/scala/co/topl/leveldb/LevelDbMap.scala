package co.topl.leveldb

import org.iq80.leveldb.DB

import scala.collection.mutable
import scala.jdk.CollectionConverters._

/**
 * An implementation of a Scala mutable.Map using LevelDB to store and retrieve the data.  This collection
 * is not threadsafe.
 */
class LevelDbMap[Key: ByteCodec, Value: ByteCodec](db: DB) extends mutable.Map[Key, Value] {

  override def iterator: Iterator[(Key, Value)] = {
    val it = db.iterator()
    it.seekToFirst()
    it.asScala
      .map(entry =>
        implicitly[ByteCodec[Key]].deserialize(entry.getKey) ->
        implicitly[ByteCodec[Value]].deserialize(entry.getValue)
      )
  }

  override def addOne(elem: (Key, Value)): this.type = {
    val batch = db.createWriteBatch()
    batch.put(
      implicitly[ByteCodec[Key]].serialize(elem._1),
      implicitly[ByteCodec[Value]].serialize(elem._2)
    )
    db.write(batch)
    batch.close()
    this
  }

  override def subtractOne(elem: Key): this.type = {
    val batch = db.createWriteBatch()
    batch.delete(
      implicitly[ByteCodec[Key]].serialize(elem)
    )
    db.write(batch)
    batch.close()
    this
  }

  override def get(key: Key): Option[Value] =
    Option(db.get(implicitly[ByteCodec[Key]].serialize(key)))
      .map(implicitly[ByteCodec[Value]].deserialize)
}

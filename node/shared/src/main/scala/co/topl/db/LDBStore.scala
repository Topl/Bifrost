package co.topl.db

import java.io.File
import java.util.concurrent.locks.ReentrantReadWriteLock
import org.iq80.leveldb._
import org.iq80.leveldb.impl.Iq80DBFactory._

/**
 * Disk storage database using LevelDB
 * @param dir database directory
 */

case class LDBStore(dir: String) {

  type K = Array[Byte]
  type V = Array[Byte]

  private val lock: ReentrantReadWriteLock = new ReentrantReadWriteLock()

  val iFile = new File(dir)
  iFile.mkdirs()
  //javaFactoryName = "org.iq80.leveldb.impl.Iq80DBFactory"
  val op = new Options()
  op.createIfMissing(true)
  op.paranoidChecks(true)
  op.blockSize(4 * 1024 * 1024)
  op.cacheSize(0)
  op.maxOpenFiles(10)
  op.compressionType(CompressionType.SNAPPY)
  var database: DB = factory.open(iFile, op)

  def refresh(): Unit = {
    lock.readLock().lock()
    try {
      database.close()
      database = factory.open(iFile, op)
    } finally lock.readLock().unlock()
  }

  def get(key: K): Option[V] = {
    lock.readLock().lock()
    val result =
      try Option(database.get(key))
      finally lock.readLock().unlock()
    result match {
      case Some(bytes: Array[Byte]) => Some(bytes)
      case _                        => None
    }
  }

  def known(key: K): Boolean =
    get(key) match {
      case Some(_) => true
      case None    => false
    }

  def update(remove: Seq[K], insert: Seq[(K, V)]): Unit = {
    val dbBatch = database.createWriteBatch()
    try {
      for (entry <- remove)
        dbBatch.delete(entry)
      for (entry <- insert)
        dbBatch.put(entry._1, entry._2)
      database.write(dbBatch)
    } finally dbBatch.close()
  }

  def close(): Unit = {
    lock.writeLock().lock()
    try database.close()
    finally lock.writeLock().unlock()
  }

}

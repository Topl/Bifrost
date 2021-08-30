package co.topl.stakeholder.primitives

import java.io.File
import java.util.concurrent.locks.ReentrantReadWriteLock
import org.iq80.leveldb._
import org.iq80.leveldb.impl.Iq80DBFactory._
import io.iohk.iodb.ByteArrayWrapper

/**
  * AMS 2020:
  * Disk storage database using LevelDB
  * this is the primary disk storage interface used for consensus
  * @param dir database directory
  */

case class LDBStore(dir:String) {

  private val lock:ReentrantReadWriteLock = new ReentrantReadWriteLock()

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
  var database:DB = factory.open(iFile, op)

  def refresh():Unit = {
    lock.readLock().lock()
    try {
      database.close()
      database = factory.open(iFile, op)
    } finally {
      lock.readLock().unlock()
    }
  }

  def get(key:ByteArrayWrapper):Option[ByteArrayWrapper] = {
    lock.readLock().lock()
    val result = try {
      Option(database.get(key.data))
    } finally {
      lock.readLock().unlock()
    }
    result match {
      case Some(bytes:Array[Byte]) => Some(ByteArrayWrapper(bytes))
      case _ => None
    }
  }

  def known(key:ByteArrayWrapper):Boolean = {
    get(key) match {
      case Some(bytes:ByteArrayWrapper) => true
      case None => false
    }
  }

  def update(remove:Seq[ByteArrayWrapper],insert:Seq[(ByteArrayWrapper,ByteArrayWrapper)]):Unit = {
    val dbBatch = database.createWriteBatch()
    try {
      for (entry <- remove) {
        dbBatch.delete(entry.data)
      }
      for (entry <- insert) {
        dbBatch.put(entry._1.data,entry._2.data)
      }
      database.write(dbBatch)
    } finally {
      dbBatch.close()
    }
  }

  def close(): Unit = {
    lock.writeLock().lock()
    try {
      database.close()
    } finally {
      lock.writeLock().unlock()
    }
  }

}

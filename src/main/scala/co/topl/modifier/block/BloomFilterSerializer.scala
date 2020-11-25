package co.topl.modifier.block

import co.topl.utils.serialization.{BifrostSerializer, Reader, Writer}

import scala.collection.BitSet

object BloomFilterSerializer extends BifrostSerializer[BloomFilter] {

  override def serialize(obj: BloomFilter, w: Writer): Unit = {
    w.putInt(obj.topics.size)
    obj.topics.foreach(t => w.putInt(t))
  }

  override def parse(r: Reader): BloomFilter = {
    val topicsLength = r.getInt()
    val topics: BitSet = BitSet((for (_ <- 0 until topicsLength) yield r.getInt()): _*)
    BloomFilter(topics)
  }
}

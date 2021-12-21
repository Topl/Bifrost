package co.topl.codecs.binary.legacy.modifier.block

import co.topl.codecs.binary.legacy.{BifrostSerializer, Reader, Writer}
import co.topl.modifier.block.BloomFilter

object BloomFilterSerializer extends BifrostSerializer[BloomFilter] {

  override def serialize(obj: BloomFilter, w: Writer): Unit =
    obj.value.foreach(l => w.putLong(l))

  override def parse(r: Reader): BloomFilter = {
    val value: Array[Long] = (for (_ <- 0 until BloomFilter.numLongs) yield r.getLong()).toArray
    new BloomFilter(value)
  }
}

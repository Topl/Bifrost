package co.topl.primitives

import com.google.common.primitives.{Ints, Longs}

/**
  * AMS 2020:
  * Simple byte array handler for serialization
  */

class ByteStream(var data:Array[Byte],co:Any) {
  def get(n:Int):Array[Byte] = {
    if (n>data.length) println("Error: ByteStream reached early end of stream")
    assert(n<=data.length)
    val out = data.take(n)
    data = data.drop(n)
    out
  }

  def getAll:Array[Byte] = {
    val out = data
    data = Array()
    out
  }

  def getInt: Int = {
    Ints.fromByteArray(get(4))
  }

  def getLong: Long = {
    Longs.fromByteArray(get(8))
  }

  def empty:Boolean = data.isEmpty

  def length:Int = data.length

  def caseObject:Any = co
}

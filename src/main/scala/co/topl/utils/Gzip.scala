package co.topl.utils

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

import scala.collection.mutable.ArrayBuffer

object Gzip {
  /**
    * Compress byte array using GZIPOutputStream in java
    * @see [[https://gist.github.com/owainlewis/1e7d1e68a6818ee4d50e]]
    * @see [[http://yangcongchufang.com/scala-gzip-class.html]]
    * @return Array[Byte]
    */
  def compress(input: Array[Byte]): Array[Byte] = {
    val bos = new ByteArrayOutputStream(input.length)
    val gzip = new GZIPOutputStream(bos)
    gzip.write(input)
    gzip.close()
    val compressed = bos.toByteArray
    bos.close()
    compressed
  }

  /**
    * Decompress byte array using GZIPInputStream in java
    * @see [[https://gist.github.com/owainlewis/1e7d1e68a6818ee4d50e]]
    * @see [[http://yangcongchufang.com/scala-gzip-class.html]]
    * @return Array[Byte]
    */
  def decompress(compressed: Array[Byte]): Array[Byte] = {
    val gzipInputStream = new GZIPInputStream(new ByteArrayInputStream(compressed))
    val output = new ArrayBuffer[Byte]()
    var totalByteCount = 0
    val bytes = new Array[Byte](1024)
    while (gzipInputStream.available() == 1) {
      val byteCount = gzipInputStream.read(bytes)
      if (byteCount > 0) {
        output ++= bytes.take(byteCount)
        totalByteCount += byteCount
      }
    }
    output.take(totalByteCount).toArray
  }
}

package bifrost.utilities

import java.io.{ByteArrayOutputStream, ByteArrayInputStream}
import java.util.zip.{GZIPOutputStream, GZIPInputStream}

import scala.util.Try
/**
  * Sourced from https://gist.github.com/owainlewis/1e7d1e68a6818ee4d50e#file-gzip-scala
  */

object Gzip {

  def compress(input: Array[Byte]): Array[Byte] = {
    val bos = new ByteArrayOutputStream(input.length)
    val gzip = new GZIPOutputStream(bos)
    gzip.write(input)
    gzip.close()
    val compressed = bos.toByteArray
    bos.close()
    compressed
  }

  def decompress(compressed: Array[Byte]): Option[Array[Byte]] =
    Try {
      val inputStream = new GZIPInputStream(new ByteArrayInputStream(compressed))
      val buffer = new Array[Byte](1024*1024)
      val out = new ByteArrayOutputStream

      var len = 0
      while ({
        len = inputStream.read(buffer)
        len > 0
      }) out.write(buffer, 0, len)

      inputStream.close()
      out.close()
      out.toByteArray
    }.toOption
}

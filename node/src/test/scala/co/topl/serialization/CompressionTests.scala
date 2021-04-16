package co.topl.serialization

import co.topl.utils.{Gzip, Logging}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

class CompressionTests extends AnyPropSpec
  with Matchers
  with Logging {

  property("A byte array (size<1024) after being compressed and decompressed should be the same as before") {
    /* Byte range is from -128 to 127 */
    val start = System.nanoTime()

    val randArray: Array[Byte] = Array.fill(15*1024)((scala.util.Random.nextInt(256) - 128).toByte)
    val compressedArray: Array[Byte] = Gzip.compress(randArray)
    randArray sameElements Gzip.decompress(compressedArray) shouldBe true

    val duration = (System.nanoTime() - start) / 1e9d
    log.debug(s"Compressed and decompressed byte array(length: 15*1024) in $duration seconds")
  }

  property("A byte array (1024<size<16*1024) after being compressed and decompressed should be the same as before") {
    /* Byte range is from -128 to 127 */
    val randArray: Array[Byte] = Array.fill(512)((scala.util.Random.nextInt(256) - 128).toByte)
    val compressedArray: Array[Byte] = Gzip.compress(randArray)
    randArray sameElements Gzip.decompress(compressedArray) shouldBe true
  }
}

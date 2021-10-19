package co.topl.serialization

import co.topl.utils.CommonGenerators
import co.topl.utils.codecs.binary.legacy.{Reader, VLQReader}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.nio.charset.StandardCharsets

class VLQReaderSpec extends AnyFlatSpec with Matchers with ScalaCheckDrivenPropertyChecks with CommonGenerators {

  val vlqReader: VLQReader = new VLQReader {
    override type CH = this.type
    override def newReader(chunk: this.type): Reader.Aux[this.type] = ???
    override def getChunk(size:   Int): this.type = ???
    override def peekByte(): Byte = ???
    override def getBoolean(): Boolean = ???
    override def getByte(): Byte = 10.toByte
    override def getBytes(size: Int): Array[Byte] = "hello".getBytes(StandardCharsets.UTF_8)
    override def mark(): this.type = ???
    override def consumed: Int = ???
    override def position: Int = ???
    override def position_=(p: Int): Unit = ???
    override def remaining: Int = ???
  }

  "VLQReader.getByteString" should "return a valid UTF-8 String" in {
    val result = vlqReader.getByteString()
    result shouldBe "hello"
  }
}

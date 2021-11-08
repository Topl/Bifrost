package co.topl.serialization

import co.topl.utils.CommonGenerators
import co.topl.codecs.binary.legacy.{VLQWriter, Writer}
import co.topl.codecs.binary.scodecs.valuetypes.Constants.stringCharacterSet
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.nio.charset.StandardCharsets

class VLQWriterSpec
    extends AnyFlatSpec
    with MockFactory
    with Matchers
    with ScalaCheckDrivenPropertyChecks
    with CommonGenerators {

  val mockVlqWriter: VLQWriter = new VLQWriter {
    override def toBytes: Array[Byte] = ???
    override type CH = this.type
    override def length(): Int = ???
    override def newWriter(): Writer.Aux[this.type] = ???
    override def putChunk(chunk: this.type): this.type = this
    override def put(x:          Byte): this.type = this
    override def putBoolean(x:   Boolean): this.type = this
    override def putBytes(xs:    Array[Byte]): this.type = this
    override def result(): this.type = this
  }

  "VLQWriter putByteString" should "throw an illegal argument exception when input bytes is length 256" in {
    val inputString = new String(Array.fill(256)(0x02).map(_.toByte), stringCharacterSet)

    an[IllegalArgumentException] should be thrownBy mockVlqWriter.putByteString(inputString)
  }

  it should "not throw an illegal argument exception when input bytes is length 255" in {
    val inputString = new String(Array.fill(255)(0x01).map(_.toByte), stringCharacterSet)

    // should not throw
    mockVlqWriter.putByteString(inputString)
  }

  it should "save bytes in UTF-8 format" in {
    val inputString = "test"

    val writer: VLQWriter = new VLQWriter {
      override def toBytes: Array[Byte] = ???
      override type CH = this.type
      override def length(): Int = ???
      override def newWriter(): Writer.Aux[this.type] = ???
      override def putChunk(chunk: this.type): this.type = this
      override def put(x:          Byte): this.type = this
      override def putBoolean(x:   Boolean): this.type = this
      override def putBytes(xs: Array[Byte]): this.type = {
        new String(xs, StandardCharsets.UTF_8) shouldBe inputString
        this
      }
      override def result(): this.type = this
    }

    writer.putByteString(inputString)
  }

  "VLQWriter putIntString"
    .should("not throw an illegal argument exception when input bytes size is 5000")
    .in {
      val inputString = new String(Array.fill(5000)(0x04).map(_.toByte), stringCharacterSet)

      // should not throw
      mockVlqWriter.putIntString(inputString)
    }

  it should "save bytes in UTF-8 format" in {
    val inputString = "test"

    val writer: VLQWriter = new VLQWriter {
      override def toBytes: Array[Byte] = ???
      override type CH = this.type
      override def length(): Int = ???
      override def newWriter(): Writer.Aux[this.type] = ???
      override def putChunk(chunk: this.type): this.type = this
      override def put(x:          Byte): this.type = this
      override def putBoolean(x:   Boolean): this.type = this
      override def putBytes(xs: Array[Byte]): this.type = {
        new String(xs, StandardCharsets.UTF_8) shouldBe inputString
        this
      }

      override def putULong(x: Long): this.type = this
      override def result(): this.type = this
    }

    writer.putIntString(inputString)
  }
}

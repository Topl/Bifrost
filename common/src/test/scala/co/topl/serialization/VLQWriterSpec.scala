package co.topl.serialization

import co.topl.utils.CommonGenerators
import co.topl.utils.serialization.{stringCharacterSet, VLQByteStringWriter}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class VLQWriterSpec extends AnyFlatSpec with Matchers with ScalaCheckDrivenPropertyChecks with CommonGenerators {

  "VLQWriter putByteString" should "throw an illegal argument exception when input bytes is length 256" in {
    val vlqWriter = new VLQByteStringWriter()
    val inputString = new String(Array.fill(256)(0x02).map(_.toByte), stringCharacterSet)

    an[IllegalArgumentException] should be thrownBy vlqWriter.putByteString(inputString)
  }

  it should "not throw an illegal argument exception when input bytes is length 255" in {
    val vlqWriter = new VLQByteStringWriter()
    val inputString = new String(Array.fill(255)(0x01).map(_.toByte), stringCharacterSet)

    // should not throw
    vlqWriter.putByteString(inputString)
  }

  "VLQWriter putIntString"
    .should("not throw an illegal argument exception when input bytes size is 5000")
    .in {
      val vlqWriter = new VLQByteStringWriter()
      val inputString = new String(Array.fill(5000)(0x04).map(_.toByte), stringCharacterSet)

      // should not throw
      vlqWriter.putIntString(inputString)
    }
}

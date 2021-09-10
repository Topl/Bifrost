package co.topl.utils.codecs.binary

import akka.util.ByteString
import co.topl.utils.CommonGenerators
import co.topl.utils.IdiomaticScalaTransition.implicits._
import co.topl.utils.serialization.{VLQByteStringReader, VLQByteStringWriter}
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}
import scodec.bits.BitVector

class IntStringCodecSpec
    extends AnyFlatSpec
    with CommonGenerators
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with Matchers {
  "IntStringCodec Decoder" should "be able to decode VLQByteStringWriter output" in {
    forAll(Gen.asciiStr) { stringValue =>
      val vlqWriter = new VLQByteStringWriter

      vlqWriter.putIntString(stringValue)

      val bytes = vlqWriter.result()

      val decoderResult = IntStringCodec.decode(BitVector(bytes)).getOrThrow()

      decoderResult.value shouldBe stringValue
      decoderResult.remainder shouldBe empty
    }
  }

  it should "return additional leftover bytes after parsing IntString" in {
    forAll(Gen.asciiStr, nonEmptyBytesGen) { (stringValue, leftover) =>
      val vlqWriter = new VLQByteStringWriter

      vlqWriter.putIntString(stringValue)

      val bytes = vlqWriter.result() ++ leftover

      val decoderResult = IntStringCodec.decode(BitVector(bytes)).getOrThrow()

      decoderResult.remainder.toByteArray shouldBe leftover
    }
  }

  "IntString Encoder" should "produce an encoded value that is decodable by VLQByteStringReader" in {
    forAll(Gen.asciiStr) { intStringValue =>
      val encodedBits = IntStringCodec.encode(intStringValue).getOrThrow()

      val encodedByteString = ByteString(encodedBits.toByteArray)

      val vlqReader = new VLQByteStringReader(encodedByteString)

      val result = vlqReader.getIntString()

      result shouldBe intStringValue
    }
  }
}

package co.topl.utils.codecs.binary

import akka.util.ByteString
import co.topl.utils.CommonGenerators
import co.topl.utils.IdiomaticScalaTransition.implicits._
import co.topl.utils.codecs.binary.legacy.{VLQByteStringReader, VLQByteStringWriter}
import co.topl.utils.codecs.binary.valuetypes.ByteStringCodec
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}
import scodec.bits.BitVector

class ByteStringCodecSpec
    extends AnyFlatSpec
    with CommonGenerators
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with Matchers {

  "SmallStringCodec Decoder" should "be able to decode VLQByteStringWriter output" in {
    forAll(Gen.asciiStr) { stringValue =>
      val vlqWriter = new VLQByteStringWriter

      vlqWriter.putByteString(stringValue)

      val bytes = vlqWriter.result()

      val decoderResult = ByteStringCodec.decode(BitVector(bytes)).getOrThrow()

      decoderResult.value shouldBe stringValue
      decoderResult.remainder shouldBe empty
    }
  }

  it should "return additional leftover bytes after parsing" in {
    forAll(Gen.asciiStr, nonEmptyBytesGen) { (stringValue, leftover) =>
      val vlqWriter = new VLQByteStringWriter

      vlqWriter.putByteString(stringValue)

      val bytes = vlqWriter.result() ++ leftover

      val decoderResult = ByteStringCodec.decode(BitVector(bytes)).getOrThrow()

      decoderResult.remainder.toByteArray shouldBe leftover
    }
  }

  "SmallStringCodec Encoder" should "produce an encoded value that is decodable by VLQByteStringReader" in {
    forAll(Gen.asciiStr) { stringValue =>
      val encodedBits = ByteStringCodec.encode(stringValue).getOrThrow()

      val encodedByteString = ByteString(encodedBits.toByteArray)

      val vlqReader = new VLQByteStringReader(encodedByteString)

      val result = vlqReader.getByteString()

      result shouldBe stringValue
    }
  }

  "SmallStringCodec" should "be able to successfully decode an encoded value" in {
    forAll(Gen.asciiStr) { value =>
      val encodedBits = ByteStringCodec.encode(value).getOrThrow()

      val decodedValue = ByteStringCodec.decode(encodedBits).getOrThrow()

      decodedValue.value shouldBe value
    }
  }
}

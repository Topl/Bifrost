package co.topl.utils.codecs.binary

import akka.util.ByteString
import co.topl.utils.CommonGenerators
import co.topl.utils.IdiomaticScalaTransition.implicits.toAttemptOps
import co.topl.utils.codecs.binary.legacy.{VLQByteStringReader, VLQByteStringWriter}
import co.topl.utils.codecs.binary.valuetypes.BytesCodec
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}
import scodec.bits.BitVector

class BytesCodecSpec
    extends AnyFlatSpec
    with CommonGenerators
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with Matchers {

  "BytesCodec Decoder" should "be able to decode VLQByteStringWriter output" in {
    forAll(nonEmptyBytesGen) { bytesValue =>
      val vlqWriter = new VLQByteStringWriter

      vlqWriter.putBytes(bytesValue)

      val bytes = vlqWriter.result()

      val decoderResult = BytesCodec.decode(BitVector(bytes), bytesValue.length).getOrThrow()

      decoderResult.value shouldBe bytesValue
      decoderResult.remainder shouldBe empty
    }
  }

  it should "return additional leftover bytes after parsing" in {
    forAll(nonEmptyBytesGen, nonEmptyBytesGen) { (value, leftover) =>
      val vlqWriter = new VLQByteStringWriter

      vlqWriter.putBytes(value)

      val bytes = vlqWriter.result() ++ leftover

      val decoderResult = BytesCodec.decode(BitVector(bytes), value.length).getOrThrow()

      decoderResult.remainder.toByteArray shouldBe leftover
    }
  }

  "BytesCodec Encoder" should "produce an encoded value that is decodable by VLQByteStringReader" in {
    forAll(nonEmptyBytesGen) { value =>
      val encodedBits = BytesCodec.encode(value).getOrThrow()

      val encodedByteString = ByteString(encodedBits.toByteArray)

      val vlqReader = new VLQByteStringReader(encodedByteString)

      val result = vlqReader.getBytes(value.length)

      result shouldBe value
    }
  }

  "BytesCodec" should "be able to successfully decode an encoded value" in {
    forAll(nonEmptyBytesGen) { value =>
      val encodedBits = BytesCodec.encode(value).getOrThrow()

      val decodedValue = BytesCodec.decode(encodedBits, value.length).getOrThrow()

      decodedValue.value shouldBe value
    }
  }
}

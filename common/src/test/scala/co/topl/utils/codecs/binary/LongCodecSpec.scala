package co.topl.utils.codecs.binary

import akka.util.ByteString
import co.topl.utils.CommonGenerators
import co.topl.utils.IdiomaticScalaTransition.implicits._
import co.topl.utils.codecs.binary.valuetypes.LongCodec
import co.topl.utils.serialization.{VLQByteStringReader, VLQByteStringWriter}
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}
import scodec.bits.BitVector

class LongCodecSpec
    extends AnyFlatSpec
    with CommonGenerators
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with Matchers {

  "LongCodec Decoder" should "be able to decode VLQByteStringWriter output" in {
    forAll(Gen.choose(Long.MinValue, Long.MaxValue)) { longValue =>
      val vlqWriter = new VLQByteStringWriter

      vlqWriter.putLong(longValue)

      val bytes = vlqWriter.result()

      val decoderResult = LongCodec.decode(BitVector(bytes)).getOrThrow()

      decoderResult.value shouldBe longValue
      decoderResult.remainder shouldBe empty
    }
  }

  it should "return additional leftover bytes after parsing" in {
    forAll(Gen.choose(Long.MinValue, Long.MaxValue), nonEmptyBytesGen) { (longValue, leftover) =>
      val vlqWriter = new VLQByteStringWriter

      vlqWriter.putLong(longValue)

      val bytes = vlqWriter.result() ++ leftover

      val decoderResult = LongCodec.decode(BitVector(bytes)).getOrThrow()

      decoderResult.remainder.toByteArray shouldBe leftover
    }
  }

  "LongCodec Encoder" should "produce an encoded value that is decodable by VLQByteStringReader" in {
    forAll(positiveLongGen) { longValue =>
      val encodedBits = LongCodec.encode(longValue).getOrThrow()

      val encodedByteString = ByteString(encodedBits.toByteArray)

      val vlqReader = new VLQByteStringReader(encodedByteString)

      val result = vlqReader.getLong()

      result shouldBe longValue
    }
  }

  "LongCodec" should "be able to successfully decode an encoded value" in {
    forAll(positiveLongGen) { value =>
      val encodedBits = LongCodec.encode(value).getOrThrow()

      val decodedValue = LongCodec.decode(encodedBits).getOrThrow()

      decodedValue.value shouldBe value
    }
  }
}

package co.topl.utils.codecs.binary

import akka.util.ByteString
import co.topl.utils.CommonGenerators
import co.topl.utils.IdiomaticScalaTransition.implicits._
import co.topl.utils.codecs.binary.legacy.{VLQByteStringReader, VLQByteStringWriter}
import co.topl.utils.codecs.binary.valuetypes.ShortCodec
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}
import scodec.bits.BitVector

class ShortCodecSpec
    extends AnyFlatSpec
    with CommonGenerators
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with Matchers {

  "ShortCodec Decoder" should "be able to decode VLQByteStringWriter output" in {
    forAll(Gen.choose(Short.MinValue, Short.MaxValue)) { shortValue =>
      val vlqWriter = new VLQByteStringWriter

      vlqWriter.putShort(shortValue)

      val bytes = vlqWriter.result()

      val decoderResult = ShortCodec.decode(BitVector(bytes)).getOrThrow()

      decoderResult.value shouldBe shortValue
      decoderResult.remainder shouldBe empty
    }
  }

  it should "return additional leftover bytes after parsing" in {
    forAll(Gen.choose(Short.MinValue, Short.MaxValue), nonEmptyBytesGen) { (shortValue, leftover) =>
      val vlqWriter = new VLQByteStringWriter

      vlqWriter.putShort(shortValue)

      val bytes = vlqWriter.result() ++ leftover

      val decoderResult = ShortCodec.decode(BitVector(bytes)).getOrThrow()

      decoderResult.remainder.toByteArray shouldBe leftover
    }
  }

  "ShortCodec Encoder" should "produce an encoded value that is decodable by VLQByteStringReader" in {
    forAll(Gen.choose(Short.MinValue, Short.MaxValue)) { shortValue =>
      val bits = ShortCodec.encode(shortValue).getOrThrow()

      val byteString = ByteString(bits.toByteArray)
      val vlqReader = new VLQByteStringReader(byteString)

      val parsedResult = vlqReader.getShort()

      parsedResult shouldBe shortValue
    }
  }

  "ShortCodec" should "be able to successfully decode an encoded value" in {
    forAll(Gen.choose(Short.MinValue, Short.MaxValue)) { value =>
      val encodedBits = ShortCodec.encode(value).getOrThrow()

      val decodedValue = ShortCodec.decode(encodedBits).getOrThrow()

      decodedValue.value shouldBe value
    }
  }
}

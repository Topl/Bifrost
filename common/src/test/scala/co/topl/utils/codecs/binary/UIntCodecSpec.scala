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

class UIntCodecSpec
    extends AnyFlatSpec
    with CommonGenerators
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with Matchers {
  "UIntCodec Decoder" should "be able to decode VLQByteStringWriter output" in {
    forAll(Gen.choose(0, Int.MaxValue)) { intValue =>
      val vlqWriter = new VLQByteStringWriter

      vlqWriter.putUInt(intValue)

      val bytes = vlqWriter.result()

      val decoderResult = UIntCodec.decode(BitVector(bytes)).getOrThrow()

      decoderResult.value shouldBe intValue
      decoderResult.remainder shouldBe empty
    }
  }

  it should "return additional leftover bytes after parsing" in {
    forAll(Gen.choose(0, Int.MaxValue), nonEmptyBytesGen) { (intValue, leftover) =>
      val vlqWriter = new VLQByteStringWriter

      vlqWriter.putUInt(intValue)

      val bytes = vlqWriter.result() ++ leftover

      val decoderResult = UIntCodec.decode(BitVector(bytes)).getOrThrow()

      decoderResult.remainder.toByteArray shouldBe leftover
    }
  }

  "UIntCodec Encoder" should "produce an encoded value that is decodable by VLQByteStringReader" in {
    forAll(Gen.choose(0, Int.MaxValue)) { intValue =>
      val bits = UIntCodec.encode(intValue).getOrThrow()

      val byteString = ByteString(bits.toByteArray)
      val vlqReader = new VLQByteStringReader(byteString)

      val parsedResult = vlqReader.getUInt()

      parsedResult shouldBe intValue
    }
  }

  "UIntCodec" should "be able to successfully decode an encoded value" in {
    forAll(Gen.choose(0, Int.MaxValue)) { value =>
      val encodedBits = UIntCodec.encode(value).getOrThrow()

      val decodedValue = UIntCodec.decode(encodedBits).getOrThrow()

      decodedValue.value shouldBe value
    }
  }
}

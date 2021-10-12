package co.topl.utils.codecs.binary

import akka.util.ByteString
import co.topl.utils.CommonGenerators
import co.topl.utils.IdiomaticScalaTransition.implicits._
import co.topl.utils.codecs.binary.valuetypes.BooleanCodec
import co.topl.utils.serialization.{VLQByteStringReader, VLQByteStringWriter}
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}
import scodec.bits.BitVector

class BooleanCodecSpec
    extends AnyFlatSpec
    with CommonGenerators
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with Matchers {

  "BooleanCodec Decoder" should "be able to decode VLQByteStringWriter output" in {
    forAll(Gen.oneOf(true, false)) { booleanValue =>
      val vlqWriter = new VLQByteStringWriter

      vlqWriter.putBoolean(booleanValue)

      val bytes = vlqWriter.result()

      val decoderResult = BooleanCodec.decode(BitVector(bytes)).getOrThrow()

      decoderResult.value shouldBe booleanValue
      decoderResult.remainder shouldBe empty
    }
  }

  it should "return additional leftover bytes after parsing" in {
    forAll(Gen.oneOf(true, false), nonEmptyBytesGen) { (booleanValue, leftover) =>
      val vlqWriter = new VLQByteStringWriter

      vlqWriter.putBoolean(booleanValue)

      val bytes = vlqWriter.result() ++ leftover

      val decoderResult = BooleanCodec.decode(BitVector(bytes)).getOrThrow()

      decoderResult.remainder.toByteArray shouldBe leftover
    }
  }

  "BooleanCodec Encoder" should "produce an encoded value that is decodable by VLQByteStringReader" in {
    forAll(Gen.oneOf(true, false)) { booleanValue =>
      val encodedBits = BooleanCodec.encode(booleanValue).getOrThrow()

      val encodedByteString = ByteString(encodedBits.toByteArray)

      val vlqReader = new VLQByteStringReader(encodedByteString)

      val result = vlqReader.getBoolean()

      result shouldBe booleanValue
    }
  }

  "BooleanCodec" should "be able to successfully decode an encoded value" in {
    forAll(Gen.oneOf(true, false)) { value =>
      val encodedBits = BooleanCodec.encode(value).getOrThrow()

      val decodedValue = BooleanCodec.decode(encodedBits).getOrThrow()

      decodedValue.value shouldBe value
    }
  }
}

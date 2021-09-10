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

class IntCodecSpec
    extends AnyFlatSpec
    with CommonGenerators
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with Matchers {
  "IntCodec Decoder" should "be able to decode VLQByteStringWriter output" in {
    forAll(Gen.choose(Int.MinValue, Int.MaxValue)) { intValue =>
      val vlqWriter = new VLQByteStringWriter

      vlqWriter.putInt(intValue)

      val bytes = vlqWriter.result()

      val decoderResult = IntCodec.decode(BitVector(bytes)).getOrThrow()

      decoderResult.value shouldBe intValue
      decoderResult.remainder shouldBe empty
    }
  }

  it should "return additional leftover bytes after parsing Int" in {
    forAll(Gen.choose(Int.MinValue, Int.MaxValue), nonEmptyBytesGen) { (intValue, leftover) =>
      val vlqWriter = new VLQByteStringWriter

      vlqWriter.putInt(intValue)

      val bytes = vlqWriter.result() ++ leftover

      val decoderResult = IntCodec.decode(BitVector(bytes)).getOrThrow()

      decoderResult.remainder.toByteArray shouldBe leftover
    }
  }

  "IntCodec Encoder" should "produce an encoded value that is decodable by VLQByteStringReader" in {
    forAll(Gen.choose(Int.MinValue, Int.MaxValue)) { intValue =>
      val encodedBits = IntCodec.encode(intValue).getOrThrow()

      val encodedByteString = ByteString(encodedBits.toByteArray)

      val vlqReader = new VLQByteStringReader(encodedByteString)

      val result = vlqReader.getInt()

      result shouldBe intValue
    }
  }
}

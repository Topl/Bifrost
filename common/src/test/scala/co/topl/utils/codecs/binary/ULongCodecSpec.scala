package co.topl.utils.codecs.binary

import akka.util.ByteString
import co.topl.utils.CommonGenerators
import co.topl.utils.IdiomaticScalaTransition.implicits._
import co.topl.utils.serialization.{VLQByteStringReader, VLQByteStringWriter}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}
import scodec.bits.BitVector

class ULongCodecSpec
    extends AnyFlatSpec
    with CommonGenerators
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with Matchers {
  "ULongCodec Decoder" should "be able to decode VLQByteStringWriter output" in {
    forAll(positiveLongGen) { longValue =>
      val vlqWriter = new VLQByteStringWriter

      vlqWriter.putULong(longValue)

      val uLongBytes = vlqWriter.result()

      val decoderResult = ULongCodec.decode(BitVector(uLongBytes)).getOrThrow()

      decoderResult.value shouldBe longValue
      decoderResult.remainder shouldBe empty
    }
  }

  it should "return additional leftover bytes after parsing ULong" in {
    forAll(positiveLongGen, nonEmptyBytesGen) { (longValue, leftover) =>
      val vlqWriter = new VLQByteStringWriter

      vlqWriter.putULong(longValue)

      val bytes = vlqWriter.result() ++ leftover

      val decoderResult = ULongCodec.decode(BitVector(bytes)).getOrThrow()

      decoderResult.remainder.toByteArray shouldBe leftover
    }
  }

  "ULongCodec Encoder" should "produce an encoded value that is decodable by VLQByteStringReader" in {
    forAll(positiveLongGen) { longValue =>
      val encodedBits = ULongCodec.encode(longValue).getOrThrow()

      val encodedByteString = ByteString(encodedBits.toByteArray)

      val vlqReader = new VLQByteStringReader(encodedByteString)

      val result = vlqReader.getULong()

      result shouldBe longValue
    }
  }
}

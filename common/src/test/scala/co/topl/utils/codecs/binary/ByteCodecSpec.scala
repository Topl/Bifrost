package co.topl.utils.codecs.binary

import akka.util.ByteString
import co.topl.utils.CommonGenerators
import co.topl.utils.IdiomaticScalaTransition.implicits.toAttemptOps
import co.topl.utils.serialization.{VLQByteStringReader, VLQByteStringWriter}
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}
import scodec.bits.BitVector

class ByteCodecSpec
    extends AnyFlatSpec
    with CommonGenerators
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with Matchers {

  val byteGen = Gen.choose(Int.MinValue, Int.MaxValue).map(_.toByte)

  "ByteCodec Decoder" should "be able to decode VLQByteStringWriter output" in {
    forAll(byteGen) { byteValue =>
      val vlqWriter = new VLQByteStringWriter

      vlqWriter.put(byteValue)

      val bytes = vlqWriter.result()

      val decoderResult = ByteCodec.decode(BitVector(bytes)).getOrThrow()

      decoderResult.value shouldBe byteValue
      decoderResult.remainder shouldBe empty
    }
  }

  it should "return additional leftover bytes after parsing" in {
    forAll(byteGen, nonEmptyBytesGen) { (value, leftover) =>
      val vlqWriter = new VLQByteStringWriter

      vlqWriter.put(value)

      val bytes = vlqWriter.result() ++ leftover

      val decoderResult = ByteCodec.decode(BitVector(bytes)).getOrThrow()

      decoderResult.remainder.toByteArray shouldBe leftover
    }
  }

  "ByteCodec Encoder" should "produce an encoded value that is decodable by VLQByteStringReader" in {
    forAll(byteGen) { value =>
      val encodedBits = ByteCodec.encode(value).getOrThrow()

      val encodedByteString = ByteString(encodedBits.toByteArray)

      val vlqReader = new VLQByteStringReader(encodedByteString)

      val result = vlqReader.getByte()

      result shouldBe value
    }
  }

  "ByteCodec" should "be able to successfully decode an encoded value" in {
    forAll(byteGen) { value =>
      val encodedBits = ByteCodec.encode(value).getOrThrow()

      val decodedValue = ByteCodec.decode(encodedBits).getOrThrow()

      decodedValue.value shouldBe value
    }
  }
}

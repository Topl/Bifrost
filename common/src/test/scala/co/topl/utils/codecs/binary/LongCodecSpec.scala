package co.topl.utils.codecs.binary

import co.topl.utils.CommonGenerators
import co.topl.utils.IdiomaticScalaTransition.implicits._
import co.topl.utils.serialization.VLQByteStringWriter
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

  it should "return additional leftover bytes after parsing Long" in {
    forAll(Gen.choose(Long.MinValue, Long.MaxValue), nonEmptyBytesGen) { (longValue, leftover) =>
      val vlqWriter = new VLQByteStringWriter

      vlqWriter.putLong(longValue)

      val bytes = vlqWriter.result() ++ leftover

      val decoderResult = LongCodec.decode(BitVector(bytes)).getOrThrow()

      decoderResult.remainder.toByteArray shouldBe leftover
    }
  }
}

package co.topl.utils.codecs.binary

import co.topl.utils.CommonGenerators
import co.topl.utils.IdiomaticScalaTransition.implicits.toEitherOps
import co.topl.utils.serialization.VLQByteStringWriter
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

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

      val decoderResult = LongCodec.decode(LazyList.from(bytes)).getOrThrow()

      decoderResult._1 shouldBe longValue
      decoderResult._2 shouldBe empty
    }
  }

  it should "return additional leftover bytes after parsing Long" in {
    forAll(Gen.choose(Long.MinValue, Long.MaxValue), nonEmptyBytesGen) { (longValue, leftover) =>
      val vlqWriter = new VLQByteStringWriter

      vlqWriter.putLong(longValue)

      val bytes = vlqWriter.result() ++ leftover

      val decoderResult = LongCodec.decode(LazyList.from(bytes)).getOrThrow()

      decoderResult._2.toArray shouldBe leftover
    }
  }
}

package co.topl.utils.codecs.binary

import co.topl.utils.CommonGenerators
import co.topl.utils.IdiomaticScalaTransition.implicits.toEitherOps
import co.topl.utils.serialization.VLQByteStringWriter
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

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

      val decoderResult = ShortCodec.decode(bytes.toList).getOrThrow()

      decoderResult._1 shouldBe shortValue
      decoderResult._2 shouldBe empty
    }
  }

  it should "return additional leftover bytes after parsing Short" in {
    forAll(Gen.choose(Short.MinValue, Short.MaxValue), nonEmptyBytesGen) { (shortValue, leftover) =>
      val vlqWriter = new VLQByteStringWriter

      vlqWriter.putShort(shortValue)

      val bytes = vlqWriter.result() ++ leftover

      val decoderResult = ShortCodec.decode(bytes.toList).getOrThrow()

      decoderResult._2.toArray shouldBe leftover
    }
  }
}

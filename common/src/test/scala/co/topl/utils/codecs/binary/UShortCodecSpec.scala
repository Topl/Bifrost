package co.topl.utils.codecs.binary

import co.topl.utils.CommonGenerators
import co.topl.utils.IdiomaticScalaTransition.implicits._
import co.topl.utils.serialization.VLQByteStringWriter
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}
import scodec.bits.BitVector

class UShortCodecSpec
    extends AnyFlatSpec
    with CommonGenerators
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with Matchers {
  "UShortCodec Decoder" should "be able to decode VLQByteStringWriter output" in {
    // TODO: make max value an int
    forAll(Gen.choose(0.toShort, Short.MaxValue)) { shortValue =>
      val vlqWriter = new VLQByteStringWriter

      vlqWriter.putUShort(shortValue)

      val bytes = vlqWriter.result()

      val decoderResult = UShortCodec.decode(BitVector(bytes)).getOrThrow()

      decoderResult.value shouldBe shortValue
      decoderResult.remainder shouldBe empty
    }
  }

  it should "return additional leftover bytes after parsing UShort" in {
    forAll(Gen.choose(0.toShort, Short.MaxValue), nonEmptyBytesGen) { (shortValue, leftover) =>
      val vlqWriter = new VLQByteStringWriter

      vlqWriter.putUShort(shortValue)

      val bytes = vlqWriter.result() ++ leftover

      val decoderResult = UShortCodec.decode(BitVector(bytes)).getOrThrow()

      decoderResult.remainder.toByteArray shouldBe leftover
    }
  }
}

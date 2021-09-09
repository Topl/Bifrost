package co.topl.utils.codecs.binary

import co.topl.utils.CommonGenerators
import co.topl.utils.IdiomaticScalaTransition.implicits._
import co.topl.utils.codecs.implicits.longDecoder
import co.topl.utils.serialization.VLQByteStringWriter
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}
import scodec.bits.BitVector

class OptionCodecSpec
    extends AnyFlatSpec
    with CommonGenerators
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with Matchers {
  "OptionCodec Decoder" should "be able to decode VLQByteStringWriter output" in {
    forAll(Gen.option(positiveLongGen)) { longOptionValue =>
      val vlqWriter = new VLQByteStringWriter

      vlqWriter.putOption(longOptionValue) { (writer, l) =>
        writer.putLong(l)
      }

      val bytes = vlqWriter.result()

      val decoderResult = OptionCodec.decode[Long](BitVector(bytes)).getOrThrow()

      decoderResult.value shouldBe longOptionValue
      decoderResult.remainder shouldBe empty
    }
  }

  it should "return additional leftover bytes after parsing Option" in {
    forAll(Gen.option(positiveLongGen), nonEmptyBytesGen) { (longOptionValue, leftover) =>
      val vlqWriter = new VLQByteStringWriter

      vlqWriter.putOption(longOptionValue) { (writer, l) =>
        writer.putLong(l)
      }

      val bytes = vlqWriter.result() ++ leftover

      val decoderResult = OptionCodec.decode[Long](BitVector(bytes)).getOrThrow()

      decoderResult.remainder.toByteArray shouldBe leftover
    }
  }
}

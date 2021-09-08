package co.topl.utils.codecs.binary

import co.topl.utils.CommonGenerators
import co.topl.utils.IdiomaticScalaTransition.implicits.toEitherOps
import co.topl.utils.serialization.VLQByteStringWriter
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

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

      val decoderResult = BooleanCodec.decode(bytes.toList).getOrThrow()

      decoderResult._1 shouldBe booleanValue
      decoderResult._2 shouldBe empty
    }
  }

  it should "return additional leftover bytes after parsing Boolean" in {
    forAll(Gen.oneOf(true, false), nonEmptyBytesGen) { (booleanValue, leftover) =>
      val vlqWriter = new VLQByteStringWriter

      vlqWriter.putBoolean(booleanValue)

      val bytes = vlqWriter.result() ++ leftover

      val decoderResult = BooleanCodec.decode(bytes.toList).getOrThrow()

      decoderResult._2.toArray shouldBe leftover
    }
  }
}

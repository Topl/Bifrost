package co.topl.utils.codecs.binary

import co.topl.utils.CommonGenerators
import co.topl.utils.IdiomaticScalaTransition.implicits.toEitherOps
import co.topl.utils.serialization.VLQByteStringWriter
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

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

      val decoderResult = IntCodec.decode(LazyList.from(bytes)).getOrThrow()

      decoderResult._1 shouldBe intValue
      decoderResult._2 shouldBe empty
    }
  }

  it should "return additional leftover bytes after parsing Int" in {
    forAll(Gen.choose(Int.MinValue, Int.MaxValue), nonEmptyBytesGen) { (intValue, leftover) =>
      val vlqWriter = new VLQByteStringWriter

      vlqWriter.putInt(intValue)

      val bytes = vlqWriter.result() ++ leftover

      val decoderResult = IntCodec.decode(LazyList.from(bytes)).getOrThrow()

      decoderResult._2.toArray shouldBe leftover
    }
  }
}

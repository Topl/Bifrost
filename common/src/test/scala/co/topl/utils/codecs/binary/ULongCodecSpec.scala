package co.topl.utils.codecs.binary

import co.topl.utils.CommonGenerators
import co.topl.utils.IdiomaticScalaTransition.implicits._
import co.topl.utils.serialization.VLQByteStringWriter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

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

      val decoderResult = ULongCodec.decode(LazyList.from(uLongBytes)).getOrThrow()

      decoderResult._1 shouldBe longValue
      decoderResult._2 shouldBe empty
    }
  }

  it should "return additional leftover bytes after parsing ULong" in {
    forAll(positiveLongGen, nonEmptyBytesGen) { (longValue, leftover) =>
      val vlqWriter = new VLQByteStringWriter

      vlqWriter.putULong(longValue)

      val bytes = vlqWriter.result() ++ leftover

      val decoderResult = ULongCodec.decode(LazyList.from(bytes)).getOrThrow()

      decoderResult._2.toArray shouldBe leftover
    }
  }
}

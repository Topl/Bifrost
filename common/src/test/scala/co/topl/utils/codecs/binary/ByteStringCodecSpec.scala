package co.topl.utils.codecs.binary

import co.topl.utils.CommonGenerators
import co.topl.utils.IdiomaticScalaTransition.implicits.toEitherOps
import co.topl.utils.serialization.VLQByteStringWriter
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class ByteStringCodecSpec
    extends AnyFlatSpec
    with CommonGenerators
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with Matchers {
  "ByteStringCodec Decoder" should "be able to decode VLQByteStringWriter output" in {
    forAll(Gen.asciiStr) { stringValue =>
      val vlqWriter = new VLQByteStringWriter

      vlqWriter.putByteString(stringValue)

      val bytes = vlqWriter.result()

      val decoderResult = ByteStringCodec.decode(LazyList.from(bytes)).getOrThrow()

      decoderResult._1 shouldBe stringValue
      decoderResult._2 shouldBe empty
    }
  }

  it should "return additional leftover bytes after parsing ByteString" in {
    forAll(Gen.asciiStr, nonEmptyBytesGen) { (stringValue, leftover) =>
      val vlqWriter = new VLQByteStringWriter

      vlqWriter.putByteString(stringValue)

      val bytes = vlqWriter.result() ++ leftover

      val decoderResult = ByteStringCodec.decode(LazyList.from(bytes)).getOrThrow()

      decoderResult._2.toArray shouldBe leftover
    }
  }
}

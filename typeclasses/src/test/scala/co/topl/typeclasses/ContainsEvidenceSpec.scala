package co.topl.typeclasses

import co.topl.codecs.bytes.scodecs._
import co.topl.codecs.bytes.typeclasses.ImmutableCodec
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.crypto.hash.Blake2b256
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, EitherValues, OptionValues}
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class ContainsEvidenceSpec
    extends AnyFlatSpec
    with BeforeAndAfterAll
    with MockFactory
    with Matchers
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with EitherValues
    with OptionValues {
  behavior of "ContainsEvidence#fromImmutableCodec"

  it should "create a ContainsEvidence instance from an ImmutableCodec instance" in {
    implicit val dataImmutableCodec: ImmutableCodec[Data] =
      ImmutableCodec.fromScodecCodec(
        (intCodec :: byteStringCodec).as[Data]
      )

    forAll { (x: Int, y: String, prefix: Byte) =>
      val underTest = ContainsEvidence.fromImmutableCodec[Data](prefix)
      val data = Data(x, y)
      val evidence = underTest.typedEvidenceOf(data)

      evidence.typePrefix shouldBe prefix
      evidence.evidence shouldBe new Blake2b256().hash(data.immutableBytes)
    }
  }
}

case class Data(x: Int, y: String)

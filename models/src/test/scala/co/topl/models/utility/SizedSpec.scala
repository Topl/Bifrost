package co.topl.models.utility

import co.topl.models.Bytes
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class SizedSpec
    extends AnyFlatSpec
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with MockFactory
    with EitherValues {

  behavior of "Sized"

  it should "enforce a strict size limit on correctly sized data" in {
    val data = Bytes(Array.fill[Byte](4)(0))

    val sized = Sized.strict[Bytes, Lengths.`4`.type](data).value

    sized.data shouldBe data
  }

  it should "enforce a max size limit on correctly sized data" in {
    val data = Bytes(Array.fill[Byte](3)(0))

    val sized = Sized.max[Bytes, Lengths.`4`.type](data).value

    sized.data shouldBe data
  }

  it should "reject strict incorrectly sized data" in {
    val data = Bytes(Array.fill[Byte](5)(0))

    val error = Sized.strict[Bytes, Lengths.`4`.type](data).left.value

    error shouldBe Sized.InvalidLength(5)
  }

  it should "reject max incorrectly sized data" in {
    val data = Bytes(Array.fill[Byte](5)(0))

    val error = Sized.max[Bytes, Lengths.`4`.type](data).left.value

    error shouldBe Sized.InvalidLength(5)
  }

  it should "accept correctly sized Int128" in {
    val bigInt = BigInt(Int.MaxValue)
    val sized = Sized.max[BigInt, Lengths.`64`.type](bigInt).value

    sized.data shouldBe bigInt
  }

  it should "reject incorrectly sized Int128" in {
    val bigInt = BigInt(Long.MaxValue)
    val error = Sized.max[BigInt, Lengths.`32`.type](bigInt).left.value

    error shouldBe Sized.InvalidLength(bigInt.bitLength)
  }

}

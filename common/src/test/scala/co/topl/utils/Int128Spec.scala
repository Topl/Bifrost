package co.topl.utils

import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class Int128Spec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with CommonGenerators with Matchers {

  property("Int128 should have a Numeric typeclass to allow .sum calls (small numbers)") {
    forAll(Gen.listOfN(10, smallInt128Gen)) { int128s =>
      // Intentionally verbose method of verifying the sum
      var expectedSum = 0: Int128
      int128s.foreach(expectedSum += _)

      int128s.sum shouldBe expectedSum
    }
  }

  property("Int128 should have an Ordering typeclass to allow .sort calls (small numbers)") {
    forAll(Gen.listOfN(10, smallInt128Gen)) { int128s =>
      val sortedInt128s = int128s.sorted

      // Intentionally verbose method of verifying sorting
      sortedInt128s.sliding(2).foreach { slide =>
        val List(smaller, larger) = slide
        smaller.intValue() should be <= larger.intValue()
      }

      // An additional/redundant verification
      sortedInt128s.map(_.intValue()) shouldBe int128s.map(_.intValue()).sorted
    }
  }

  property("Int128 should have a Numeric typeclass to allow .sum calls (large numbers)") {
    val int128s = List(
      new Int128(3, 0),
      new Int128(2, 0),
      new Int128(1, 0)
    )

    int128s.sum shouldBe new Int128(6, 0)
  }

  property("Int128 should have an Ordering typeclass to allow .sort calls (large numbers)") {
    val int128s = List(
      new Int128(3, 0),
      new Int128(2, 0),
      new Int128(1, 0)
    )

    val sortedInt128s = int128s.sorted

    sortedInt128s shouldBe List(
      new Int128(1, 0),
      new Int128(2, 0),
      new Int128(3, 0)
    )

    sortedInt128s.max shouldBe new Int128(3, 0)
    sortedInt128s.min shouldBe new Int128(1, 0)
  }

}

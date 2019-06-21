package bifrost.program

import bifrost.{BifrostGenerators, ValidGenerators}
import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}

import scala.util.{Failure, Try}


class FulfilmentFunctionSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

  property("FulfilmentFunction should error if there is no 0 point") {
    forAll(positiveTinyIntGen) {
      i =>
        forAll(seqLongDoubleGen(i).suchThat(!_.exists(_._1 == 0))) {
          seq => Try {
            PiecewiseLinearSingle(seq)
          } shouldBe a[Failure[_]]
        }
    }
  }

  property("FulfilmentFunction should error if there are multiple points with the same x value") {
    forAll(positiveTinyIntGen) {
      i =>
        forAll(seqLongDoubleGen(i)) {
          seq =>
            val idx = Gen.choose(0, seq.length).sample.get
            Try {
              PiecewiseLinearSingle(seq :+ seq(idx)._1 -> samplePositiveDouble)
            } shouldBe a[Failure[_]]
        }
    }
  }

  property("FulfilmentFunction should error if there is a point with delivery value which is less than 0") {
    forAll(positiveTinyIntGen) {
      i =>
        forAll(seqLongDoubleGen(i)) {
          seq =>
            Try {
              PiecewiseLinearSingle(seq :+ positiveLongGen.sample.get -> -1 * samplePositiveDouble)
            } shouldBe a[Failure[_]]
        }
    }
  }

  //property("FulfilmentFunction should interpolate a line properly") {}


}
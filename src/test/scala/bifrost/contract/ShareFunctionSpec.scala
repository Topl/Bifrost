package bifrost.contract

import bifrost.{BifrostGenerators, ValidGenerators}
import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}

import scala.util.{Failure, Try}


class ShareFunctionSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

  property("ShareFunction should error if there is no 0 point") {
    forAll(positiveTinyIntGen) {
      i =>
        forAll(seqLongDoubleGen(i).suchThat(!_.exists(_._1 == 0))) {
          seq => Try {
            PiecewiseLinearSingle(seq)
          } shouldBe a[Failure[_]]
        }
    }
  }

  property("ShareFunction should error if there are multiple points with the same x value") {
    forAll(positiveTinyIntGen) {
      i =>
        forAll(seqDoubleGen(i)) {
          seq =>
            val idx = Gen.choose(0, seq.length).sample.get
            val first = samplePositiveDouble / 2
            val second = samplePositiveDouble / 2
            Try {
              PiecewiseLinearMultiple(seq :+ seq(idx)._1 -> (first, second, 1 - first - second))
            } shouldBe a[Failure[_]]
        }
    }
  }

  property("ShareFunction should error if no 0 point") {}
  property("ShareFunction should error if there is any point where shares don't add up to 100%") {
    forAll(positiveTinyIntGen) {
      i =>
        forAll(seqDoubleGen(i)) {
          seq =>
            val idx = Gen.choose(0, seq.length).sample.get
            val first = samplePositiveDouble / 3
            val second = samplePositiveDouble / 3
            val third = samplePositiveDouble / 3
            Try {
              PiecewiseLinearMultiple(seq :+ seq(idx)._1 -> (first, second, third))
            } shouldBe a[Failure[_]]
        }
    }
  }

  property("ShareFunction should error if there is any point where negative") {
    forAll(positiveTinyIntGen) {
      i =>
        forAll(seqDoubleGen(i)) {
          seq =>
            val multipliers = Gen
              .oneOf(Seq((-1, 1, 1), (1, -1, 1), (1, 1, -1)))
              .sample
              .get

            val first = samplePositiveDouble / 2
            val second = samplePositiveDouble / 2

            Try {
              PiecewiseLinearMultiple(seq :+ samplePositiveDouble -> (
                multipliers._1 * first,
                multipliers._2 * second,
                multipliers._3 * (1 - first - second)
              )
              )
            } shouldBe a[Failure[_]]

        }
    }
  }

  property("ShareFunction should interpolate lines properly") {}
}
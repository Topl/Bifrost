package co.topl.genus.typeclasses.mongofilter

import co.topl.genus.typeclasses.MongoFilter
import org.scalacheck.Arbitrary
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

trait MongoFilterBehavior extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  def testMongoFilterBehavior[T: MongoFilter: Arbitrary](testName: String): Unit = {
    behavior of testName

    it should "not fail when converting to filter JSON string" in {
      forAll { (value: T) =>
        MongoFilter[T]
          .toBsonFilter(value)
          .toBsonDocument
          .toJson
      }
    }

    it should "not result in an empty JSON string" in {
      forAll { (value: T) =>
        val result = MongoFilter[T]
          .toBsonFilter(value)
          .toBsonDocument
          .toJson

        result shouldNot be(empty)
      }
    }
  }
}

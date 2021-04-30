package co.topl.crypto.hash

import co.topl.crypto.utils.Base58
import org.scalacheck.Gen
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import co.topl.crypto.Implicits._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Sha256Spec extends AnyPropSpec with ScalaCheckPropertyChecks {

  lazy val stringGen: Gen[String] = Gen.alphaNumStr.suchThat(_.nonEmpty)

  val testVectors = List(
    "test"  -> "Bjj4AWTNrjQVHqgWbP2XaxXz4DYH1WZMyERHxsad7b2w",
    "topl"  -> "AJnLg7Nq42rdLVyRWFmLphwKhMwPsZZHKyoS5rtHJwaF",
    "scala" -> "BtRjv8Ts5d7jFLGmCcuvMjrw56NUoUxTGaPFADZTvf9f",
    ""      -> "GKot5hBsd81kMupNCXHaqbhv3huEbxAFMLnpcX2hniwn"
  )

  property("all test vectors should hash to expected") {
    testVectors.foreach { v =>
      val result: Digest32 = sha256(v._1)

      Base58.encode(result) shouldBe v._2
    }
  }

  property("should hash to length 32") {
    forAll(stringGen) { value =>
      val result = sha256(value)

      result.value.length shouldBe 32
    }
  }
}

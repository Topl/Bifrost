package co.topl.crypto.hash

import org.scalatest.propspec.AnyPropSpec
import co.topl.crypto.Implicits._
import co.topl.crypto.utils.Base58
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class Blake2b256Spec extends AnyPropSpec with ScalaCheckPropertyChecks {

  lazy val stringGen: Gen[String] = Gen.alphaNumStr.suchThat(_.nonEmpty)

  val testVectors = List(
    "test"  -> "As3ZuwnL9LpoW3wz8HoDpHtZqJ4dhPFFnv87GYrnCYKj",
    "topl"  -> "EASZj3hxx47a8N81aAUjRKDK2GfZf5J2D4p9mdt1DUh3",
    "scala" -> "EQAX2eVMsKtWD7ioiRSnU8kckB44QazXkfNbwEgzFFoD",
    ""      -> "xyw95Bsby3s4mt6f4FmFDnFVpQBAeJxBFNGzu2cX4dM" // WHAT?!
  )

  property("all test vectors should hash to expected with Blake2b256") {
    testVectors.foreach { v =>
      val result: Digest32 = Blake2b256.digest32.hash(None, v._1)

      Base58.encode(result) shouldBe v._2
    }
  }

  property("should hash to length 32") {
    forAll(stringGen) { value =>
      val result = Blake2b256.digest32.hash(None, value)

      result.value.length shouldBe 32
    }
  }
}

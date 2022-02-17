package co.topl.utils.encode

import co.topl.utils.IdiomaticScalaTransition.implicits._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Base16Spec extends AnyFlatSpec {

  private val testVectors = Seq(
    ""       -> "",
    "f"      -> "66",
    "fo"     -> "666f",
    "foo"    -> "666f6f",
    "foob"   -> "666f6f62",
    "fooba"  -> "666f6f6261",
    "foobar" -> "666f6f626172"
  )

  "Base-16" should "pass test vectors when encoding" in {
    testVectors.foreach { t =>
      Base16.encode(t._1.getBytes("UTF-8")) shouldBe t._2
    }
  }

  "Base-16" should "pass test vectors when decoding" in {
    testVectors.foreach { t =>
      Base16.decode(t._2).getOrThrow() shouldBe t._1.getBytes("UTF-8")
    }
  }
}

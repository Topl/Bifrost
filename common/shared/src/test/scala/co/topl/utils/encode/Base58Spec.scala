package co.topl.utils.encode

import co.topl.utils.IdiomaticScalaTransition.implicits._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Base58Spec extends AnyFlatSpec {

  private val testVectors = Seq(
    "Hello World!"                                 -> "2NEpo7TZRRrLZSi2U",
    "The quick brown fox jumps over the lazy dog." -> "USm3fpXnKG5EUBx2ndxBDMPVciP5hGey2Jh4NDv6gmeo1LkMeiKrLJUUBk6Z"
  )

  "Base-58" should "pass test vectors when encoding" in {
    testVectors.foreach { t =>
      Base58.encode(t._1.getBytes("UTF-8")) shouldBe t._2
    }
  }

  "Base-58" should "pass test vectors when decoding" in {
    testVectors.foreach { t =>
      Base58.decode(t._2).getOrThrow() shouldBe t._1.getBytes("UTF-8")
    }
  }
}

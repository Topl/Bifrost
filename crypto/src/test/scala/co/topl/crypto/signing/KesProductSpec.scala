package co.topl.crypto.signing

import co.topl.models.Bytes
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class KesProductSpec
    extends AnyFlatSpec
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with Matchers {

  "KesProduct" should "verify a message signed with the appropriate public key" in {
    forAll { (seed1: Bytes, seed2: Bytes, message1: Bytes, message2: Bytes, height: (Int, Int)) =>
      whenever(!(seed1 == seed2) && !(message1 == message2) && height._1 <= 5 && height._2 <= 5) {
        val kesProduct = new KesProduct
        val (sk1, vk1) = kesProduct.createKeyPair(seed1, (5, 5), 0)
        val (_, vk2) = kesProduct.createKeyPair(seed2, (5, 5), 0)
        val sig = kesProduct.sign(sk1, message1)

        kesProduct.verify(sig, message1, vk1) shouldBe true
        kesProduct.verify(sig, message1, vk2) shouldBe false
        kesProduct.verify(sig, message2, vk1) shouldBe false
      }
    }
  }
  it should "generate identical keypairs given the same seed" in {
    forAll { (seedBytes: Bytes, height: (Int, Int)) =>
      whenever(seedBytes.toArray.length != 0 && height._1 <= 10 && height._2 <= 10) {
        val kesProduct = new KesProduct
        val (sk1, vk1) = kesProduct.createKeyPair(seedBytes, height, 0)
        val (sk2, vk2) = kesProduct.createKeyPair(seedBytes, height, 0)

        sk1 shouldBe sk2
        vk1 shouldBe vk2
      }
    }
  }
}

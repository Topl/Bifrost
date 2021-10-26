package co.topl.crypto.mnemonic

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class MnemonicToExtendedEd25519     extends AnyFlatSpec
  with ScalaCheckPropertyChecks
  with ScalaCheckDrivenPropertyChecks
  with Matchers {

//  "Key from mnemonic phrase" should "output same the key with the same password" in {
//    val createKey =
//      derive[Password => ExtendedPrivateKeyEd25519](
//        "ozone drill grab fiber curtain grace pudding thank cruise elder eight picnic",
//        Mnemonic12,
//        English
//      ).getOrThrow()
//
//    forAll(stringGen) { password =>
//      val firstAttempt = createKey(password)
//      val secondAttempt = createKey(password)
//
//      firstAttempt.leftKey shouldBe secondAttempt.leftKey
//      firstAttempt.rightKey shouldBe secondAttempt.rightKey
//      firstAttempt.chainCode shouldBe secondAttempt.chainCode
//    }
//  }
//
//  it should "output a different key with a different password" in {
//
//    val createKey =
//      derive[Password => ExtendedPrivateKeyEd25519](
//        "ozone drill grab fiber curtain grace pudding thank cruise elder eight picnic",
//        Mnemonic12,
//        English
//      ).getOrThrow()
//
//    forAll(stringGen, stringGen) { (password1, password2) =>
//      val firstAttempt = createKey(password1)
//      val secondAttempt = createKey(password2)
//
//      if (password1 != password2) {
//        firstAttempt.leftKey should not be secondAttempt.leftKey
//        firstAttempt.rightKey should not be secondAttempt.rightKey
//        firstAttempt.chainCode should not be secondAttempt.chainCode
//      }
//    }
//  }
}

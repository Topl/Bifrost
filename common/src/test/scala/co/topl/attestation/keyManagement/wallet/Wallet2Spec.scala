package co.topl.attestation.keyManagement.wallet

import org.scalatest.flatspec.AnyFlatSpec
import co.topl.utils.encode.Base16
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Wallet2Spec extends AnyFlatSpec {
  "Wallet" should "pass root secret key test" in {
    val walletRoot = Wallet2.fromSeed(Base16.decode("578d685d20b602683dc5171df411d3e2").get)

    walletRoot.leftNumber.toString shouldBe "38096432269777187972282727382530464140043628323029465813805073381215192153792"

    walletRoot.rightNumber.toString shouldBe "4064253ffefc4127489bce1b825a47329010c5afb4d21154ef949ef786204405"
  }
}

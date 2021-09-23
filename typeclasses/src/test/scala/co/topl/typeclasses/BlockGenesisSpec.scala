package co.topl.typeclasses

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{EitherValues, Inspectors}

class BlockGenesisSpec extends AnyFlatSpec with Matchers with EitherValues with Inspectors {

  behavior of "BlockGenesis"

  private val block = BlockGenesis(Nil).value

  it should "have timestamp 0" in {
    block.headerV2.timestamp shouldBe 0L
  }

  it should "have slot 0" in {
    block.headerV2.slot shouldBe 0L
  }

  it should "have height 1" in {
    block.headerV2.height shouldBe 1L
  }

  it should "have all zeros for the address" in {
    val address =
      block.headerV2.address

    forAll(address.stakingVerificationKey.data)(_ shouldBe (0: Byte))
    forAll(address.signature.data)(_ shouldBe (0: Byte))
    forAll(address.paymentVerificationKeyHash.data)(_ shouldBe (0: Byte))
  }

  it should "have all zeros for the VRF Certificate" in {
    val cert = block.headerV2.eligibibilityCertificate
    forAll(cert.vkVRF.ed25519.bytes.data)(_ shouldBe (0: Byte))
    forAll(cert.testProof.bytes.data)(_ shouldBe (0: Byte))
    forAll(cert.nonceProof.bytes.data)(_ shouldBe (0: Byte))
  }

  it should "have all zeros for the KES Certificate" in {
    val cert = block.headerV2.operationalCertificate
    forAll(cert.vkKES.bytes.data)(_ shouldBe (0: Byte))
    forAll(cert.kesProof.signature.data)(_ shouldBe (0: Byte))
    forAll(cert.mmmProof.sigi)(_ shouldBe (0: Byte))
    cert.vkKES.offset shouldBe 0L
  }

  it should "have all zeros for the thresholdEvidence" in {
    forAll(block.headerV2.thresholdEvidence.data.dataBytes)(_ shouldBe (0: Byte))
  }

}

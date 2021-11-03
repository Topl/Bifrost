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

  it should "have all zeros for the Eligibility Certificate, except for the eta value" in {
    val cert = block.headerV2.eligibilityCertificate
    forAll(cert.vkVRF.bytes.data)(_ shouldBe (0: Byte))
    forAll(cert.vrfNonceSig.bytes.data)(_ shouldBe (0: Byte))
    forAll(cert.vrfTestSig.bytes.data)(_ shouldBe (0: Byte))
    forAll(cert.thresholdEvidence.data.dataBytes)(_ shouldBe (0: Byte))
  }

  // TODO
  ignore should "have all zeros for the Operational Certificate" in {}

}

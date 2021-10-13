package co.topl.consensus

import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

trait LocalChainSpec
    extends AnyFlatSpec
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with MockFactory
    with EitherValues {
  behavior of "LocalChain"

  it should "indicate when a new tine is worse than the local chain" in {}
  it should "adopt a new tine when instructed" in {}
  it should "store the head of the local canonical tine" in {}
}

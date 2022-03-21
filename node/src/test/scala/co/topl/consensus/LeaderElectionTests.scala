package co.topl.consensus

import co.topl.attestation.Address
import co.topl.attestation.implicits._
import co.topl.consensus.NxtLeaderElection.{NoAddressesAvailable, NoArbitBoxesAvailable}
import co.topl.utils.StringDataTypes.Base58Data
import co.topl.utils.{NodeGenerators, TestSettings}
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks.forAll

class LeaderElectionTests extends AnyFlatSpec with MockFactory with TestSettings with NodeGenerators with EitherValues {

  val address: Address =
    Base58Data.unsafe("AUAvJqLKc8Un3C6bC4aj8WgHZo74vamvX8Kdm6MhtdXgw51cGfix").decodeAddress.toEither.value

  "getEligibleBox" should "return NoAddressesAvailable when no addresses provided" in {
    forAll(blockCurve25519Gen, protocolVersionerGen) { (parent, protocolVersioner) =>
      val stateReader = mock[NxtLeaderElection.SR]
      val addresses = Set[Address]()
      val leaderElection = new NxtLeaderElection(protocolVersioner)
      val expectedResult = Left(NoAddressesAvailable)

      val result = NxtLeaderElection.getEligibleBox(
        parent,
        addresses,
        parent.timestamp + 100,
        NxtConsensus.State(10000000, parent.difficulty, 0L, parent.height),
        leaderElection,
        stateReader
      )

      result shouldBe expectedResult
    }
  }

  "getEligibleBox" should "return NoArbitBoxesAvailable when no addresses contain arbit boxes" in {
    forAll(blockCurve25519Gen, protocolVersionerGen) { (parent, protocolVersioner) =>
      val stateReader = mock[NxtLeaderElection.SR]
      (stateReader.getTokenBoxes _)
        .expects(address)
        .returns(None)
      val addresses = Set[Address](address)
      val leaderElection = new NxtLeaderElection(protocolVersioner)
      val expectedResult = Left(NoArbitBoxesAvailable)

      val result = NxtLeaderElection.getEligibleBox(
        parent,
        addresses,
        parent.timestamp + 100,
        NxtConsensus.State(10000000, parent.difficulty, 0L, parent.height),
        leaderElection,
        stateReader
      )

      result shouldBe expectedResult
    }
  }

}

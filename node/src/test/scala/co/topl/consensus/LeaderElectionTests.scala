package co.topl.consensus

import co.topl.attestation.Address
import co.topl.attestation.implicits._
import co.topl.consensus.ConsensusVariables.ConsensusParams
import co.topl.consensus.LeaderElection.{NoAddressesAvailable, NoArbitBoxesAvailable}
import co.topl.utils.StringDataTypes.Base58Data
import co.topl.utils.{CommonGenerators, TestSettings}
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks.forAll

class LeaderElectionTests extends AnyFlatSpec with MockFactory with CommonGenerators with EitherValues {

  implicit val nxtLeaderElection: NxtLeaderElection = NxtLeaderElection(TestSettings.defaultSettings)

  val address: Address =
    Base58Data.unsafe("AUAvJqLKc8Un3C6bC4aj8WgHZo74vamvX8Kdm6MhtdXgw51cGfix").decodeAddress.toEither.value

  "getEligibleBox" should "return NoAddressesAvailable when no addresses provided" in {
    forAll(blockCurve25519Gen) { parent =>
      val stateReader = mock[LeaderElection.SR]
      val addresses = Set[Address]()
      val expectedResult = Left(NoAddressesAvailable)

      val result = LeaderElection.getEligibleBox(
        parent,
        addresses,
        parent.timestamp + 100,
        ConsensusParams(10000000, parent.difficulty, 0L, parent.height),
        stateReader
      )

      result shouldBe expectedResult
    }
  }

  "getEligibleBox" should "return NoArbitBoxesAvailable when no addresses contain arbit boxes" in {
    forAll(blockCurve25519Gen) { parent =>
      val stateReader = mock[LeaderElection.SR]
      (stateReader.getTokenBoxes _)
        .expects(address)
        .returns(None)
      val addresses = Set[Address](address)
      val expectedResult = Left(NoArbitBoxesAvailable)

      val result = LeaderElection.getEligibleBox(
        parent,
        addresses,
        parent.timestamp + 100,
        ConsensusParams(10000000, parent.difficulty, 0L, parent.height),
        stateReader
      )

      result shouldBe expectedResult
    }
  }

}

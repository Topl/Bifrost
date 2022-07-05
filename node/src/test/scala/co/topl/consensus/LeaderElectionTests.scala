package co.topl.consensus

import co.topl.attestation.Address
import co.topl.attestation.implicits._
import co.topl.consensus.NxtLeaderElection.{NoAddressesAvailable, NoArbitBoxesAvailable}
import co.topl.utils.NodeGenerators
import co.topl.utils.StringDataTypes.Base58Data
import co.topl.utils.implicits.toEitherOps
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks.forAll

class LeaderElectionTests extends AnyFlatSpec with MockFactory with NodeGenerators with EitherValues {

  val address: Address =
    Base58Data.unsafe("AUAvJqLKc8Un3C6bC4aj8WgHZo74vamvX8Kdm6MhtdXgw51cGfix").decodeAddress.toEither.value

  "collectArbitBoxes" should "return NoAddressesAvailable when no addresses provided" in {
    val stateReader = mock[NxtLeaderElection.SR]
    val addresses = Set[Address]()

    val expectedResult = Left(NoAddressesAvailable)

    val result = NxtLeaderElection.collectArbitBoxes(addresses, stateReader)

    result shouldBe expectedResult
  }

  "getEligibleBox" should "return NoArbitBoxesAvailable when no addresses contain arbit boxes" in {
    forAll(blockCurve25519Gen) { parent =>
      val addresses = nonEmptySetAddressGen.sample.get
      val stateReader = mock[NxtLeaderElection.SR]
      (stateReader.getTokenBoxes _)
        .expects(*)
        .anyNumberOfTimes()
        .returns(None)
      val leaderElection = new NxtLeaderElection(ProtocolVersioner.default)
      val arbitBoxIterator = NxtLeaderElection.collectArbitBoxes(addresses, stateReader).getOrThrow()

      val expectedResult = Left(NoArbitBoxesAvailable)

      val result = NxtLeaderElection.getEligibleBox(
        leaderElection.calculateHitValue(parent)(_),
        leaderElection.calculateThresholdValue(
          0,
          NxtConsensus.State.empty.height,
          NxtConsensus.State.empty.difficulty,
          NxtConsensus.State.empty.totalStake
        )(_)
      )(arbitBoxIterator)

      result shouldBe expectedResult
    }
  }
}

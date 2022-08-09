package co.topl.consensus

import cats.data.NonEmptyChain
import co.topl.consensus.BlockValidators._
import co.topl.modifier.block.Block
import co.topl.settings.Version
import co.topl.utils.InMemoryKeyRingTestHelper
import co.topl.utils.TimeProvider.Time
import io.circe.Encoder
import org.scalacheck.Gen
import org.scalatest.Suite
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.forAll

import scala.util.Try

class ValidBlockchainGeneratorSpec extends AnyPropSpec with ValidBlockchainGenerator with InMemoryKeyRingTestHelper {
  self: Suite =>

  private class BlockDataExtractors(blockchain: NonEmptyChain[Block]) {
    val blockByHeight: Long => Option[Block] = height => blockchain.find(_.height == height)
    val getParent: Block => Option[Block] = (block: Block) => blockByHeight(block.height - 1)
    val getParentHeight: Block => Option[Long] = (block: Block) => getParent(block).map(_.height)

    val getDetails: Block => Some[(Block, IndexedSeq[Time])] = (block: Block) =>
      Some(
        getParent(block).get -> (Math.max(1, block.height - 3) to block.height)
          .map(blockByHeight(_).get)
          .map(_.timestamp)
      )
    val getParentTimestamp: Block => Option[Time] = (block: Block) => getParent(block).map(_.timestamp)

    val consensusState: Block => NxtConsensus.State = (block: Block) => NxtConsensus.State(Int.MaxValue, 0L)
  }

  property("Generates a valid blockchain from genesis with up to 127 blocks in length") {
    forAll(Gen.choose[Byte](2, Byte.MaxValue)) { length =>
      val leaderElection = new NxtLeaderElection(ProtocolVersioner.default)

      val genesisStrat = GenesisProvider.Strategies.Generation(Version.initial, Int.MaxValue, Long.MaxValue)

      val nonEmptyBlockchain =
        validChainFromGenesis(
          keyRingCurve25519,
          genesisStrat.balanceForEachParticipant,
          genesisStrat.initialDifficulty,
          ProtocolVersioner.default,
          Long.MaxValue / length
        )(length).sample.get

      val extractors = new BlockDataExtractors(nonEmptyBlockchain.head.block +: nonEmptyBlockchain.tail)
      import extractors._

      val validators: Block => Seq[Try[Unit]] = (block: Block) =>
        Seq(
          new DifficultyValidator(leaderElection).validate(getDetails)(block),
          new HeightValidator().validate(getParentHeight)(block),
          new EligibilityValidator(leaderElection, consensusState(block).totalStake).validate(getParent)(block),
          new SyntaxValidator(consensusState(block).inflation).validate(identity)(block),
          new TimestampValidator().validate(getParentTimestamp)(block)
        )

      // check that every validator for every block beyond genesis is valid
      nonEmptyBlockchain.tail.forall(validators(_).forall(_.isSuccess)) shouldBe true
    }
  }
}

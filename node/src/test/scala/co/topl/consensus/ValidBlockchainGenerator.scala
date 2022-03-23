package co.topl.consensus

import cats.data.NonEmptyChain
import co.topl.attestation.Address
import co.topl.attestation.keyManagement.{KeyRing, KeyfileCurve25519, KeyfileCurve25519Companion, PrivateKeyCurve25519}
import co.topl.consensus.BlockValidators._
import co.topl.modifier.block.Block
import co.topl.modifier.box.ArbitBox
import co.topl.modifier.transaction.{ArbitTransfer, PolyTransfer, TransferTransaction}
import co.topl.utils.NetworkPrefixTestHelper
import co.topl.utils.TimeProvider.Time
import co.topl.utils.implicits.toEitherOps
import org.scalacheck.Gen

import scala.util.Try

trait ValidBlockchainGenerator extends NetworkPrefixTestHelper {

  def validChainFromGenesis(
    keyRing:           KeyRing[PrivateKeyCurve25519, KeyfileCurve25519],
    protocolVersioner: ProtocolVersioner)(
    lengthOfChain:     Byte
  ): Gen[NonEmptyChain[Block]] = {
    val leaderElection = new NxtLeaderElection(protocolVersioner)
    val totalStake = Int.MaxValue
    val initialDifficulty = Long.MaxValue // ensure that the threshold calculation is maximized

    // manipulate the time between subsequent blocks to manage the adjustment of difficulty
    def timeBetweenBlocksAt(height: Long): Long =
      protocolVersioner.applicable(height).value.targetBlockTime.toMillis

    val genesis = GenesisProvider.construct(
      Set(keyRing.addresses.head), // use a single address so that the generator box is constant
      totalStake,
      initialDifficulty,
      protocolVersioner.applicable(1).blockVersion
    )

    val allArbitBoxesIterator: Iterator[ArbitBox] = genesis.block.transactions
      .collect { case transaction: TransferTransaction[_, _] =>
        transaction.newBoxes.collect { case box: ArbitBox => box }.toSeq
      }
      .flatten
      .iterator

    val eligibleBox = NxtLeaderElection
      .getEligibleBox(
        leaderElection.calculateHitValue(genesis.block)(_),
        leaderElection.calculateThresholdValue(timeBetweenBlocksAt(1), genesis.state)(_)
      )(allArbitBoxesIterator)
      .getOrThrow(e => new Exception(e.toString))

    (2 to lengthOfChain).foldLeft(NonEmptyChain(genesis.block)) { case (chain, height) =>
      val newTimestamp = chain.last.timestamp + timeBetweenBlocksAt(height)
      appendBlock(
        chain,
        keyRing,
        Address(eligibleBox.evidence),
        eligibleBox,
        chain.last,
        leaderElection.calculateNewDifficulty(
          height,
          chain.last.difficulty,
          chain.toChain.toList.takeRight(3).map(_.timestamp) :+ newTimestamp
        ),
        newTimestamp,
        protocolVersioner.applicable(height).blockVersion
      )
    }
  }

  private def appendBlock(
    blockchain:        NonEmptyChain[Block],
    keyRing:           KeyRing[PrivateKeyCurve25519, KeyfileCurve25519],
    blockProducer:     Address,
    generatorBox:      ArbitBox,
    parent:            Block,
    newBaseDifficulty: Long,
    newTimestamp:      Long,
    newBlockVersion:   Byte
  ): NonEmptyChain[Block] = {

    val rewards = {
      val base = Rewards(Nil, blockProducer, parent.id, newTimestamp, 0L).get
      base.map {
        case b: PolyTransfer[_] =>
          b.copy(attestation = keyRing.generateAttestation(keyRing.addresses)(b.messageToSign))
        case a: ArbitTransfer[_] =>
          a.copy(attestation = keyRing.generateAttestation(keyRing.addresses)(a.messageToSign))
      }
    }

    blockchain.append(
      Block
        .createAndSign(
          parent.id,
          timestamp = newTimestamp,
          txs = rewards,
          generatorBox = generatorBox,
          publicKey = keyRing.lookupPublicKey(blockProducer).get,
          height = parent.height + 1,
          difficulty = newBaseDifficulty,
          version = newBlockVersion
        )(keyRing.signWithAddress(blockProducer))
        .get
    )
  }

}

/** This is an example usage of the ValidBlockchainGenerator and a set of validator extractors for using the BlockValidators
 * to verify the chain of blocks */
object ChainTest extends ValidBlockchainGenerator {

  val keyRingCurve25519: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519] = {
    implicit def keyfileCurve25519Companion: KeyfileCurve25519Companion.type = KeyfileCurve25519Companion
    KeyRing.empty[PrivateKeyCurve25519, KeyfileCurve25519]()
  }

  keyRingCurve25519.generateNewKeyPairs(3)

  val exampleChain: NonEmptyChain[Block] = validChainFromGenesis(keyRingCurve25519, ProtocolVersioner.default)(15).sample.get
  val blockByHeight: Time => Option[Block] = (height: Long) => exampleChain.find(_.height == height)

  val leaderElection = new NxtLeaderElection(ProtocolVersioner.default)

  val getParent: Block => Option[Block] = (block: Block) => blockByHeight(block.height - 1)
  val getParentHeight: Block => Option[Long] = (block: Block) => getParent(block).map(_.height)
  val getDetails: Block => Some[(Block, IndexedSeq[Time])] = (block: Block) =>
    Some(getParent(block).get -> (Math.max(1, block.height - 3) to block.height).map(blockByHeight(_).get).map(_.timestamp))
  val getParentTimestamp: Block => Option[Time] = (block: Block) => getParent(block).map(_.timestamp)

  val consensusState: Block => NxtConsensus.State = (block: Block) => NxtConsensus.State(Int.MaxValue, getParent(block).get.difficulty, 0L, getParent(block).get.height)

  val validators: Block => Seq[Try[Unit]] = (block: Block) =>
    Seq(
      new DifficultyValidator(leaderElection).validate(getDetails)(block),
      new HeightValidator().validate(getParentHeight)(block),
      new EligibilityValidator(leaderElection, consensusState(block)).validate(getParent)(block),
      new SyntaxValidator(consensusState(block)).validate(identity)(block),
      new TimestampValidator().validate(getParentTimestamp)(block)
    )

  def main(args: Array[String]): Unit = {
    exampleChain.tail.map { block =>
      println(s"height: ${block.height}, validation -> ${validators(block).map(_.isSuccess)}")
    }
  }
}

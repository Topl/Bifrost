package co.topl.nodeView

import cats.data.NonEmptyChain
import co.topl.attestation.Address
import co.topl.attestation.keyManagement.{KeyRing, KeyfileCurve25519, KeyfileCurve25519Companion, PrivateKeyCurve25519}
import co.topl.consensus._
import co.topl.modifier.block.Block
import co.topl.modifier.box.ArbitBox
import co.topl.modifier.transaction.{ArbitTransfer, PolyTransfer, TransferTransaction}
import co.topl.utils.NetworkPrefixTestHelper
import co.topl.utils.implicits.toEitherOps
import org.scalacheck.Gen

trait ValidBlockchainGenerator extends NetworkPrefixTestHelper {

  def validChainFromGenesis(
    keyRing:           KeyRing[PrivateKeyCurve25519, KeyfileCurve25519],
    protocolVersioner: ProtocolVersioner,
    lengthOfChain:     Byte
  ): Gen[NonEmptyChain[Block]] = {
    val leaderElection = new NxtLeaderElection(protocolVersioner)
    val totalStake = Int.MaxValue
    val initalDifficulty = Long.MaxValue // ensure that the threshold calculation is maximized

    // manipulate the time between subsequent blocks to manage the adjustment of difficulty
    def timeBetweenBlocksAt(height: Long): Long =
      protocolVersioner.applicable(height).value.targetBlockTime.toMillis + 2

    val genesis = GenesisProvider.construct(
      Set(keyRing.addresses.head), // use a single address so that the generator box is constant
      totalStake,
      initalDifficulty,
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
        genesis.block,
        allArbitBoxesIterator,
        genesis.block.timestamp +
          timeBetweenBlocksAt(height = 2), // reuse the generator box by keeping the threshold near upper limit
        totalStake,
        leaderElection
      )
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
          chain.toChain.toList.takeRight(3).map(_.timestamp).reverse :+ newTimestamp
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

object ChainTest extends ValidBlockchainGenerator {

  val keyRingCurve25519: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519] = {
    implicit def keyfileCurve25519Companion: KeyfileCurve25519Companion.type = KeyfileCurve25519Companion
    KeyRing.empty[PrivateKeyCurve25519, KeyfileCurve25519]()
  }

  keyRingCurve25519.generateNewKeyPairs(3)

  def main(args: Array[String]): Unit = {
    val g = validChainFromGenesis(keyRingCurve25519, ProtocolVersioner.default, 10).sample.get

    val leaderElection = new NxtLeaderElection(ProtocolVersioner.default)
    val consensusState = NxtConsensus.State(Int.MaxValue, g.last.difficulty, 0L, g.last.height)

    def getDetailsForLast(block: Block) = g.last -> g.toChain.toList.takeRight(4).map(_.timestamp)
    def getParentTimestamp(block: Block) = g.reverse.tail.headOption.map(_.timestamp)

    val diffRes =
      new DifficultyBlockValidator(leaderElection, consensusState).validate(getDetailsForLast)(g.last).isSuccess
    val synRes = new SyntaxBlockValidator(consensusState).validate(identity)(g.last).isSuccess
    val tsRes = new TimestampValidator().validate(getParentTimestamp)(g.last).isSuccess

    println(s"${Console.YELLOW}difficulty: $diffRes, syntax: $synRes, timestamp: $tsRes${Console.RESET}")
    println(
      s"ids: ${g.map(_.id)}" +
      s"\ntimestamps: ${g.map(_.timestamp)}" +
      s"\ndifficulty: ${g.map(_.difficulty)}" +
      s"\nheight: ${g.map(_.height)}"
    )
    println(s"last 4: ${g.toChain.toList.takeRight(4).map(_.id)}")

  }
}

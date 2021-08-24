package co.topl.fullnode

import cats.Id
import cats.data.OptionT
import co.topl.consensus.{ChainSelectionChain, LeaderElection}
import co.topl.minting.Mint.ops._
import co.topl.minting.{BlockMint, StakingKeys}
import co.topl.models.HasLength.implicits._
import co.topl.models.Lengths._
import co.topl.models._
import co.topl.typeclasses.BlockGenesis
import co.topl.typeclasses.Identifiable.Instances._
import co.topl.typeclasses.Identifiable.ops._

import scala.concurrent.duration._

object FullNode extends App {

  val slotTime = 10.millis
  val SlotsPerEpoch = 5000
  val RelativeStake = Ratio(1, 10)

  val leaderElectionConfig = LeaderElection
    .Config(lddCutoff = 0, precision = 16, baselineDifficulty = Ratio(1, 15), amplitude = Ratio(2, 5))

  val genesisBlock =
    BlockGenesis(Nil).create()

  private val epoch: Epoch = 1
  private val epochNonce: Nonce = Bytes(Array(1))

  private val stakingKeys: StakingKeys[Id] =
    new StakingKeys[Id] {

      private val _vrfKey =
        Secrets.Ed25519(
          PrivateKeys.Ed25519(Sized.strict[Bytes, Lengths.`32`.type](Bytes(Array.fill[Byte](32)(1))).toOption.get),
          PublicKeys.Ed25519(Sized.strict[Bytes, Lengths.`32`.type](Bytes(Array.fill[Byte](32)(1))).toOption.get)
        )

      private val _kesKey =
        Secrets.Ed25519(
          PrivateKeys.Ed25519(Sized.strict[Bytes, Lengths.`32`.type](Bytes(Array.fill[Byte](32)(1))).toOption.get),
          PublicKeys.Ed25519(Sized.strict[Bytes, Lengths.`32`.type](Bytes(Array.fill[Byte](32)(1))).toOption.get)
        )

      override def vrfKey(): Id[Secrets.Ed25519] = _vrfKey

      override def kesKey(): Id[Secrets.Ed25519] = _kesKey

      override def stakingAddress(): Id[TaktikosAddress] = ???

      override def evolveKes(): Id[Unit] = ???
    }

  implicit val mint: BlockMint[Id] = {
    val key = Secrets.Ed25519(
      PrivateKeys.Ed25519(Sized.strict[Bytes, Lengths.`32`.type](Bytes(Array.fill[Byte](32)(1))).toOption.get),
      PublicKeys.Ed25519(Sized.strict[Bytes, Lengths.`32`.type](Bytes(Array.fill[Byte](32)(1))).toOption.get)
    )
    def elect(parent: BlockHeaderV2) = {
      val startTime = System.currentTimeMillis()
      val hit = LeaderElection
        .hits(key, RelativeStake, fromSlot = parent.slot, epochNonce, leaderElectionConfig)
        .head

      val slotDiff = hit.slot - parent.slot
      Thread.sleep(((slotDiff.toLong * slotTime.toMillis) - (System.currentTimeMillis() - startTime)).max(0L))
      BlockMint.Election(
        slot = hit.slot,
        hit.cert,
        kesCertificate = Bytes(Array.fill(32)(1))
      )
    }
    new BlockMint[Id](
      getCurrentTime = () => System.currentTimeMillis(),
      nextTransactions = _ => Nil,
      elect = parent => elect(parent)
    )
  }

  implicit val bigIntHasLength: HasLength[BigInt] = _.bitLength

  private val chainSelectionState = new ChainSelectionState(genesisBlock)

  val chainSelectionChainImpl: ChainSelectionChain[Id, Id, Throwable] =
    ChainSelectionChain[Id, Id, Throwable](
      latestBlockId = genesisBlock.headerV2.id,
      firstBlockId = genesisBlock.headerV2.id,
      nextBlockId = None,
      currentBlock = chainSelectionState.currentBlock,
      getBlock = id => chainSelectionState.headersMap(id),
      childIdOf = parentId => OptionT.fromOption(chainSelectionState.headerChildMap.get(parentId)),
      totalStake = () => chainSelectionState.totalStake().toOption.get,
      stakeFor = address => chainSelectionState.stakeMap.get(address),
      epochNonce = () => chainSelectionState.epochNonce,
      append = header => chainSelectionState.append(header),
      removeLatest = () => chainSelectionState.removeLatest()
    )

  val blockChainIterator =
    Iterator.iterate(chainSelectionChainImpl) { impl =>
      val BlockV2(newHeader, newBody) =
        BlockV2(impl.currentBlock, chainSelectionState.currentBlockBody).nextValue

      chainSelectionState.currentBlockBody = newBody
      impl
        .appendToLatest(newHeader)
        .valueOr(f => throw new Exception(f.toString))
    }

  blockChainIterator
    .takeWhile(_.currentBlock.slot <= SlotsPerEpoch)
    .foreach { impl =>
      println(
        s"Applied headerId=${new String(impl.currentBlock.id.dataBytes.toArray)}" +
        s" to parentHeaderId=${new String(impl.currentBlock.parentHeaderId.dataBytes.toArray)}" +
        s" at height=${impl.currentBlock.height}" +
        s" at slot=${impl.currentBlock.slot}" +
        s" at timestamp=${impl.currentBlock.timestamp}"
      )
    }

  println("Completed epoch")

}

class ChainSelectionState(genesisBlock: BlockV2) {

  var headersMap: Map[TypedIdentifier, BlockHeaderV2] = Map.empty
  var headerChildMap: Map[TypedIdentifier, TypedIdentifier] = Map.empty
  var stakeMap: Map[Address, Int128] = Map.empty
  var epochNonce: Nonce = Bytes(Array(1))
  var currentBlock = genesisBlock.headerV2
  var currentBlockBody = genesisBlock.blockBodyV2

  def totalStake()(implicit
    hasLength: HasLength[BigInt]
  ): Either[Sized.InvalidLength, Sized.Max[BigInt, Lengths.`128`.type]] =
    Sized.max(stakeMap.values.map(_.data).sum)

  def append(header: BlockHeaderV2) = {
    headersMap += (header.id           -> header)
    headerChildMap += (currentBlock.id -> header.id)
    currentBlock = header
  }

  def removeLatest() = {
    headersMap -= currentBlock.id
    headerChildMap -= currentBlock.parentHeaderId
    currentBlock = headersMap(currentBlock.parentHeaderId)
  }
}

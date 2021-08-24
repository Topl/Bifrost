package co.topl.fullnode

import cats.Id
import cats.data.{EitherT, OptionT, State}
import co.topl.consensus.{ChainSelectionChain, LeaderElection}
import co.topl.minting.BlockMint
import co.topl.minting.Mint.ops._
import co.topl.models._
import co.topl.models.utility.HasLength.implicits._
import co.topl.models.utility.Lengths._
import co.topl.models.utility._
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

  private val taktikosAddress: TaktikosAddress =
    TaktikosAddress(
      Sized.strict[Bytes, Lengths.`32`.type](Bytes(Array.fill[Byte](32)(1))).toOption.get,
      Sized.strict[Bytes, Lengths.`32`.type](Bytes(Array.fill[Byte](32)(1))).toOption.get,
      Sized.strict[Bytes, Lengths.`64`.type](Bytes(Array.fill[Byte](64)(1))).toOption.get
    )

  private val vrfKey =
    Secrets.Vrf(
      PrivateKeys.Vrf(
        PrivateKeys.Ed25519(Sized.strict[Bytes, Lengths.`32`.type](Bytes(Array.fill[Byte](32)(1))).toOption.get)
      ),
      PublicKeys.Vrf(
        PublicKeys.Ed25519(Sized.strict[Bytes, Lengths.`32`.type](Bytes(Array.fill[Byte](32)(1))).toOption.get)
      )
    )

  private val initialKesKey =
    Secrets.Kes(
      PrivateKeys.Kes(Sized.strict[Bytes, Lengths.`32`.type](Bytes(Array.fill[Byte](32)(1))).toOption.get),
      PublicKeys.Kes(Sized.strict[Bytes, Lengths.`32`.type](Bytes(Array.fill[Byte](32)(1))).toOption.get, 0)
    )

  private val kesKey: State[(Secrets.Kes, Long), Secrets.Kes] =
    State { v =>
      if (v._2 % 10 == 0) {
        // TODO: Persist to disk
      }
      (v._1, v._2 + 1) -> v._1
    }

  implicit val mint: BlockMint[Id] = {
    def elect(parent: BlockHeaderV2) = {
      val key = kesKey.runA(initialKesKey -> 0).value
      val startTime = System.currentTimeMillis()
      val hit = LeaderElection
        .hits(vrfKey, RelativeStake, fromSlot = parent.slot, epochNonce, leaderElectionConfig)
        .head

      val slotDiff = hit.slot - parent.slot
      Thread.sleep(((slotDiff.toLong * slotTime.toMillis) - (System.currentTimeMillis() - startTime)).max(0L))
      BlockMint.Election(
        slot = hit.slot,
        hit.cert
      )
    }
    new BlockMint[Id](
      address = taktikosAddress,
      getCurrentTime = () => System.currentTimeMillis(),
      nextTransactions = _ => Nil,
      elect = parent => elect(parent),
      nextKesCertificate = slot => {
        val secret = kesKey.runA(initialKesKey -> 0).value
        KesCertificate(
          secret.publicKey,
          Sized.strict[Bytes, Lengths.`64`.type](Bytes(Array.fill[Byte](64)(0))).toOption.get,
          Sized.strict[Bytes, Lengths.`1440`.type](Bytes(Array.fill[Byte](1440)(0))).toOption.get,
          slotOffset = slot
        )
      }
    )
  }

  private val chainSelectionState = new ChainSelectionState(genesisBlock)

  val chainSelectionChainImpl: ChainSelectionChain[Id, Throwable] =
    ChainSelectionChain[Id, Throwable](
      latestBlockId = genesisBlock.headerV2.id,
      firstBlockId = genesisBlock.headerV2.id,
      nextBlockId = None,
      currentBlock = chainSelectionState.currentBlock,
      getBlock = id => chainSelectionState.headersMap(id),
      childIdOf = parentId => OptionT.fromOption(chainSelectionState.headerChildMap.get(parentId)),
      state = chainSelectionState
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

class ChainSelectionState(genesisBlock: BlockV2) extends ChainSelectionChain.State[Id, Throwable] {
  override type S = this.type
  var headersMap: Map[TypedIdentifier, BlockHeaderV2] = Map.empty
  var headerChildMap: Map[TypedIdentifier, TypedIdentifier] = Map.empty
  var stakeMap: Map[TaktikosAddress, Int128] = Map.empty

  def epochNonce: EitherT[Id, Throwable, Bytes] = EitherT.pure(Bytes(Array(1)))
  var currentBlock = genesisBlock.headerV2
  var currentBlockBody = genesisBlock.blockBodyV2

  def totalStake(): Sized.Max[BigInt, Lengths.`128`.type] =
    Sized.max[BigInt, Lengths.`128`.type](stakeMap.values.map(_.data).sum).toOption.get

  override def relativeStakeFor(address: TaktikosAddress): EitherT[Id, Throwable, Int128] =
    EitherT.pure {
      Sized.max[BigInt, Lengths.`128`.type](stakeMap(address).data / stakeMap.values.map(_.data).sum).toOption.get
    }

  def apply(header: BlockHeaderV2): EitherT[Id, Throwable, S] =
    EitherT.pure {
      headersMap += (header.id           -> header)
      headerChildMap += (currentBlock.id -> header.id)
      currentBlock = header
      this
    }

  def unapply(): EitherT[Id, Throwable, S] = EitherT.pure {
    headersMap -= currentBlock.id
    headerChildMap -= currentBlock.parentHeaderId
    currentBlock = headersMap(currentBlock.parentHeaderId)
    this
  }
}

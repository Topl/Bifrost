package co.topl.fullnode

import cats.Id
import cats.data.{NonEmptyChain, OptionT}
import co.topl.consensus.ConsensusValidation.implicits._
import co.topl.consensus.crypto.Vrf
import co.topl.consensus.{ConsensusValidation, LeaderElection}
import co.topl.crypto.hash.blake2b256
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

  val stakerRelativeStake =
    Ratio(1, 10)

  implicit val leaderElectionConfig: LeaderElection.Config =
    LeaderElection
      .Config(lddCutoff = 0, precision = 16, baselineDifficulty = Ratio(1, 15), amplitude = Ratio(2, 5))

  implicit val vrf: Vrf =
    new Vrf

  private val Right(taktikosAddress: TaktikosAddress) =
    for {
      paymentVerificationKeyHash <- Sized.strict[Bytes, Lengths.`32`.type](Bytes(Array.fill[Byte](32)(1)))
      stakingVerificationKey     <- Sized.strict[Bytes, Lengths.`32`.type](Bytes(Array.fill[Byte](32)(1)))
      signature                  <- Sized.strict[Bytes, Lengths.`64`.type](Bytes(Array.fill[Byte](64)(1)))
    } yield TaktikosAddress(paymentVerificationKeyHash, stakingVerificationKey, signature)

  val Right(stakerEvidence) =
    Sized.strict[TypedBytes, Lengths.`33`.type](
      TypedBytes(1: Byte, Bytes(blake2b256.hash(taktikosAddress.stakingVerificationKey.data.toArray).value))
    )

  private val Right(vrfKey) =
    for {
      privateKey <- Sized
        .strict[Bytes, Lengths.`32`.type](Bytes(Array.fill[Byte](32)(1)))
        .map(PrivateKeys.Ed25519(_))
        .map(PrivateKeys.Vrf)
      publicKey <- Sized
        .strict[Bytes, Lengths.`32`.type](Bytes(Array.fill[Byte](32)(1)))
        .map(PublicKeys.Ed25519(_))
        .map(PublicKeys.Vrf)
    } yield KeyPairs.Vrf(privateKey, publicKey)

  private val Right(initialKesKey) =
    for {
      privateKey <- Sized
        .strict[Bytes, Lengths.`32`.type](Bytes(Array.fill[Byte](32)(1)))
        .map(PrivateKeys.Kes(_))
      publicKey <- Sized
        .strict[Bytes, Lengths.`32`.type](Bytes(Array.fill[Byte](32)(1)))
        .map(PublicKeys.Kes(_, slot = 0))
    } yield Secrets.Kes(privateKey, publicKey)

  implicit def mint(implicit inMemoryChain: InMemoryChain): BlockMint[Id] = {
    val interpreter = new BlockMint.Algebra[Id] {
      def address: TaktikosAddress = taktikosAddress

      def currentTime(): Timestamp = System.currentTimeMillis()

      def unconfirmedTransactions(block: BlockV2): Id[Seq[Transaction]] = Nil

      def elect(parent: BlockHeaderV2): BlockMint.Election = {
        val startTime = System.currentTimeMillis()
        val hit = LeaderElection
          .hits(
            vrfKey,
            inMemoryChain.relativeStake(stakerEvidence),
            fromSlot = parent.slot,
            inMemoryChain.epochNonce
          )
          .head

        val slotDiff = hit.slot - parent.slot
        Thread.sleep(((slotDiff * slotTime.toMillis) - (System.currentTimeMillis() - startTime)).max(0L))
        BlockMint.Election(
          slot = hit.slot,
          hit.cert,
          hit.threshold
        )
      }

      def nextKesCertificate(slot: Slot): Id[KesCertificate] = {
        val secret = initialKesKey
        KesCertificate(
          secret.publicKey,
          Proofs.Consensus.KesCertificate(
            Sized.strict[Bytes, Lengths.`64`.type](Bytes(Array.fill[Byte](64)(0))).toOption.get
          ),
          Proofs.Consensus.MMM(Sized.strict[Bytes, Lengths.`1440`.type](Bytes(Array.fill[Byte](1440)(0))).toOption.get),
          slotOffset = slot
        )
      }
    }
    new BlockMint[Id](interpreter)
  }

  val initialState =
    InMemoryChain(
      NonEmptyChain(BlockGenesis(Nil).value),
      Map(stakerEvidence -> stakerRelativeStake),
      Bytes(Array.fill[Byte](4)(0))
    )

  val blockChainIterator =
    Iterator.iterate(initialState) { implicit state =>
      val newBlock @ BlockV2(newHeader, newBody) =
        state.head.nextValue

      newHeader
        .validatedUsing[Id](
          new ConsensusValidation.Algebra[Id] {
            override def epochNonce: Id[Nonce] = state.epochNonce

            override def parentBlockHeader: Id[BlockHeaderV2] = state.head.headerV2

            override def relativeStakeFor(evidence: Evidence): OptionT[Id, Ratio] =
              OptionT.fromOption(state.relativeStake.get(evidence))
          }
        )
        .valueOr(f => throw new Exception(f.toString))

      state.append(newBlock)
    }

  blockChainIterator
    .takeWhile(_.head.headerV2.slot <= SlotsPerEpoch)
    .foreach { impl =>
      println(
        s"Applied headerId=${new String(impl.head.headerV2.id.dataBytes.toArray)}" +
        s" to parentHeaderId=${new String(impl.head.headerV2.parentHeaderId.dataBytes.toArray)}" +
        s" at height=${impl.head.headerV2.height}" +
        s" at slot=${impl.head.headerV2.slot}" +
        s" at timestamp=${impl.head.headerV2.timestamp}"
      )
    }

  println("Completed epoch")

}

case class InMemoryChain(headers: NonEmptyChain[BlockV2], relativeStake: Map[Evidence, Ratio], epochNonce: Bytes) {
  def append(nextBlock: BlockV2): InMemoryChain = copy(headers.append(nextBlock))
  def head: BlockV2 = headers.last
}

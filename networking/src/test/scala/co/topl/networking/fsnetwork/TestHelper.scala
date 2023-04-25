package co.topl.networking.fsnetwork

import cats.data.NonEmptyChain
import cats.implicits._
import co.topl.brambl.generators.TransactionGenerator
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.codecs.bytes.tetra.instances._
import co.topl.consensus.models.{BlockHeader, BlockId, SlotData}
import co.topl.models.ModelGenerators.GenHelper
import co.topl.models.generators.consensus.ModelGenerators
import co.topl.node.models.BlockBody
import org.scalacheck.{Arbitrary, Gen}
import org.scalamock.handlers.{CallHandler1, CallHandler2, CallHandler3}
import co.topl.typeclasses.implicits._

import scala.annotation.tailrec

object TestHelper extends TransactionGenerator {

  implicit class CallHandler1Ops[T1, R](ch: CallHandler1[T1, R]) {
    def rep(count: Int): CallHandler1[T1, R] = ch.repeated(count to count)
  }

  implicit class CallHandler2Ops[T1, T2, R](ch: CallHandler2[T1, T2, R]) {
    def rep(count: Int): CallHandler2[T1, T2, R] = ch.repeated(count to count)
  }

  implicit class CallHandler3Ops[T1, T2, T3, R](ch: CallHandler3[T1, T2, T3, R]) {
    def rep(count: Int): CallHandler3[T1, T2, T3, R] = ch.repeated(count to count)
  }

  @tailrec
  private def addHeaderToChain(
    headers: NonEmptyChain[BlockHeader],
    gen:     Gen[BlockHeader],
    count:   Long
  ): NonEmptyChain[BlockHeader] =
    count match {
      case 0 => headers
      case _ =>
        val parentId = headers.last.id
        addHeaderToChain(headers.append(gen.sample.get.copy(parentHeaderId = parentId)), gen, count - 1)
    }

  def arbitraryLinkedBlockHeaderChain(sizeGen: Gen[Long]): Arbitrary[NonEmptyChain[BlockHeader]] =
    Arbitrary(
      for {
        size <- sizeGen
        root <- ModelGenerators.arbitraryHeader.arbitrary
      } yield addHeaderToChain(NonEmptyChain.one(root), ModelGenerators.arbitraryHeader.arbitrary, size)
    )

  val maxTxsCount = 5

  implicit val arbitraryTxsAndBlock: Arbitrary[(Seq[IoTransaction], BlockBody)] =
    Arbitrary(
      for {
        txs <- Gen.listOfN(maxTxsCount, arbitraryIoTransaction.arbitrary)
      } yield (txs, BlockBody.of(txs.map(tx => tx.id)))
    )

  def headerToSlotData(header: BlockHeader): SlotData = {
    val sampleSlotData = ModelGenerators.arbitrarySlotData.arbitrary.first
    val slotId = sampleSlotData.slotId.copy(blockId = header.id)
    val parentSlotId = sampleSlotData.parentSlotId.copy(blockId = header.parentHeaderId)
    sampleSlotData.copy(slotId = slotId, parentSlotId = parentSlotId)
  }

  def arbitraryLinkedSlotDataHeaderBlockNoTx(
    sizeGen: Gen[Long]
  ): Arbitrary[NonEmptyChain[(BlockId, SlotData, BlockHeader, BlockBody)]] =
    Arbitrary(
      for {
        size    <- sizeGen
        headers <- arbitraryLinkedBlockHeaderChain(Gen.oneOf(List[Long](size))).arbitrary
      } yield NonEmptyChain
        .fromSeq(headers.foldLeft(List.empty[(BlockId, SlotData, BlockHeader, BlockBody)]) { case (blocks, header) =>
          val body = co.topl.models.generators.node.ModelGenerators.arbitraryNodeBody.arbitrary.first
          val headerWithTxRoot = header.copy(txRoot = body.merkleTreeRootHash.data)
          if (blocks.isEmpty) {
            List((headerWithTxRoot.id, headerToSlotData(headerWithTxRoot), headerWithTxRoot, body))
          } else {
            val headerWithParent = headerWithTxRoot.copy(parentHeaderId = blocks.last._2.slotId.blockId)
            blocks.appended((headerWithParent.id, headerToSlotData(headerWithParent), headerWithParent, body))
          }
        })
        .get
    )
}

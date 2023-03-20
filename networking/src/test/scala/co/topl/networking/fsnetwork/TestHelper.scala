package co.topl.networking.fsnetwork

import cats.data.NonEmptyChain
import co.topl.brambl.generators.TransactionGenerator
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.codecs.bytes.tetra.instances._
import co.topl.consensus.models.BlockHeader
import co.topl.models.generators.consensus.ModelGenerators
import co.topl.node.models.BlockBody
import org.scalacheck.{Arbitrary, Gen}
import org.scalamock.handlers.{CallHandler1, CallHandler2, CallHandler3}

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
}

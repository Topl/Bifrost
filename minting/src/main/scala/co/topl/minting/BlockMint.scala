package co.topl.minting

import cats._
import cats.syntax.functor._
import cats.syntax.flatMap._
import co.topl.models._
import co.topl.typeclasses.Identifiable.Instances._
import co.topl.typeclasses.Identifiable.ops._

/**
 * A `Mint` which produces Blocks.
 * @param getCurrentTime A function which fetches the current timestamp
 * @param nextTransactions A function which fetches a collection of transactions that are valid based on the
 *                         provided (head/parent) Block. This should include "Reward" transactions.
 * @param elect A function which asynchronously determines the slot and certificates for the next Block
 */
class BlockMint[F[_]](
  getCurrentTime:   () => Timestamp,
  nextTransactions: BlockV2 => F[Seq[Transaction]],
  elect:            BlockHeaderV2 => F[BlockMint.Election]
)(implicit fMonad:  Monad[F])
    extends Mint[F, BlockV2] {

  override def nextValueAfter(parentBlock: BlockV2): F[BlockV2] =
    elect(parentBlock.headerV2).flatMap { case BlockMint.Election(slot, vrfCertificate, kesCertificate) =>
      nextTransactions(parentBlock).map { transactions =>
        val timestamp = getCurrentTime()
        val body = BlockBodyV2(
          transactions = transactions,
          parentHeaderId = parentBlock.headerV2.id
        )
        val header = BlockHeaderV2(
          parentHeaderId = parentBlock.headerV2.id,
          blockBodyId = body.id,
          timestamp = timestamp,
          height = parentBlock.headerV2.height + 1,
          slot = slot,
          vrfCertificate = vrfCertificate,
          kesCertificate = kesCertificate
        )
        BlockV2(header, body)
      }
    }
}

object BlockMint {
  // TODO: `kesCertificate` isn't part of the election process
  case class Election(slot: Slot, vrfCertificate: VrfCertificate, kesCertificate: KesCertificate)
}

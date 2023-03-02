package co.topl.ledger.interpreters

import cats.effect.Sync
import cats.implicits._
import co.topl.brambl.models.TransactionOutputAddress
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.codecs.bytes.tetra.instances._
import co.topl.consensus.models.BlockId
import co.topl.ledger.algebras.BoxStateAlgebra

object AugmentedBoxState {

  /**
   * Constructs a BoxState that wraps another underlying BoxState.  The new BoxState intercepts calls to boxExistsAt
   * and checks to see if the values are included in the given StateAugmentation object.  StateAugmentation acts as
   * a simple in-memory tracker of BoxIds that have been spent and created.  It is intended to allow box existence
   * checks while considering the history of transactions _within_ the block that is being validated
   * @param boxState an underlying BoxState
   * @param stateAugmentation a set of BoxState changes to consider when intercepting box existence checks
   */
  def make[F[_]: Sync](boxState: BoxStateAlgebra[F])(stateAugmentation: StateAugmentation): F[BoxStateAlgebra[F]] =
    Sync[F].delay {
      new BoxStateAlgebra[F] {
        def boxExistsAt(blockId: BlockId)(boxId: TransactionOutputAddress): F[Boolean] =
          if (stateAugmentation.newBoxIds.contains(boxId)) true.pure[F]
          else if (stateAugmentation.spentBoxIds.contains(boxId)) false.pure[F]
          else boxState.boxExistsAt(blockId)(boxId)
      }
    }

  /**
   * Establishes an ephemeral set of spent box IDs and new box IDs.  This augmentation can be further augmented
   * by including an additional transaction.
   * @param spentBoxIds a set of box IDs that are no longer eligibile to be spent
   * @param newBoxIds a set of box IDs that have been created _and_ have not been spent
   */
  case class StateAugmentation(spentBoxIds: Set[TransactionOutputAddress], newBoxIds: Set[TransactionOutputAddress]) {

    /**
     * Returns a new StateAugmentation which includes the inputs and outputs of the given transaction.  If
     * the Transaction spends a box that exists in `newBoxIds`, the entry is moved from `newBoxIds` to `spentBoxIds`.
     */
    def augment(transaction: IoTransaction): StateAugmentation = {
      val transactionSpentBoxIds = transaction.inputs.map(_.address.getIoTransaction32).toSet
      val transactionId = transaction.id
      val transactionNewBoxIds =
        transaction.outputs.mapWithIndex((_, idx) => BoxId(transactionId, idx.toShort)).toSet
      StateAugmentation(
        spentBoxIds ++ transactionSpentBoxIds,
        (newBoxIds ++ transactionNewBoxIds) -- transactionSpentBoxIds
      )
    }

  }

  object StateAugmentation {
    val empty: StateAugmentation = StateAugmentation(Set.empty, Set.empty)
  }
}

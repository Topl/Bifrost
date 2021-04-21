package co.topl.nodeView.state

import co.topl.modifier.block.Block
import co.topl.modifier.box.{Box, BoxId}

import scala.util.Try

case class StateChanges(override val boxIdsToRemove: Seq[BoxId], override val toAppend: Seq[Box[_]])
    extends GenericStateChanges[Box[_]](boxIdsToRemove, toAppend)

object StateChanges {

  def apply(mod: Block): Try[StateChanges] = Try {
    // extract the needed box data from all transactions within a block and
    // aggregate the transaction data into separate lists for updating state
    val (toRemove, toAdd) =
      mod.transactions
        .map { tx =>
          (tx.boxIdsToOpen, tx.newBoxes)
        }
        .foldLeft((Seq[BoxId](), Seq[Box[_]]()))((aggregate, boxDelta) => {
          (
            aggregate._1 ++ boxDelta._1,
            aggregate._2 ++ boxDelta._2
          )
        })

    // return the state changes that can be applied
    new StateChanges(toRemove, toAdd)
  }
}

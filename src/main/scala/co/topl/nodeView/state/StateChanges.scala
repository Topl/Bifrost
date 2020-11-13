package co.topl.nodeView.state

import co.topl.modifier.block.Block
import co.topl.nodeView.state.box.{ Box, BoxId }

import scala.util.Try

case class StateChanges( override val boxIdsToRemove: Set[BoxId],
                         override val toAppend: Set[Box[_]],
                       ) extends GenericStateChanges[Any, Box[_]](boxIdsToRemove, toAppend)

object StateChanges {
  type BX = Box[_]
  type BPMOD = Block

  def apply(mod: BPMOD): Try[StateChanges] = Try {
      // extract the needed box data from all transactions within a block and
      // aggregate the transaction data into separate lists for updating state
      val (toRemove: Set[BoxId], toAdd: Set[BX]) =
        mod.transactions.map {
          tx => (tx.boxIdsToOpen.toSet, tx.newBoxes.toSet)
        }.foldLeft((Set[BoxId](), Set[BX]()))(
          (aggregate, boxDelta) => {
            (
              aggregate._1 ++ boxDelta._1,
              aggregate._2 ++ boxDelta._2
            )
          }
        )

      // return the state changes that can be applied
      new StateChanges(toRemove, toAdd)
    }
}

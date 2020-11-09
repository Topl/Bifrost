package co.topl.nodeView.state

import co.topl.nodeView.state.box.{ BoxId, GenericBox }

class GenericStateChanges[T, BX <: GenericBox[T]](val boxIdsToRemove: Set[BoxId], val toAppend: Set[BX])


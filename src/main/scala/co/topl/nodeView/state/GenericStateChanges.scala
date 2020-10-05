package co.topl.nodeView.state

import co.topl.nodeView.state.box.{ BoxId, GenericBox }
import co.topl.nodeView.state.box.proposition.Proposition

class GenericStateChanges[T, P <: Proposition, BX <: GenericBox[P, T]](val boxIdsToRemove: Set[BoxId], val toAppend: Set[BX])


package co.topl.nodeView.state

import co.topl.nodeView.box.GenericBox
import co.topl.nodeView.box.proposition.Proposition
import co.topl.nodeView.box.GenericBox
import co.topl.nodeView.box.proposition.Proposition

class GenericStateChanges[T, P <: Proposition, BX <: GenericBox[P, T]](val boxIdsToRemove: Set[Array[Byte]], val toAppend: Set[BX])


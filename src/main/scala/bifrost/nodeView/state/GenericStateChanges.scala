package bifrost.nodeView.state

import bifrost.nodeView.box.GenericBox
import bifrost.nodeView.box.proposition.Proposition

class GenericStateChanges[T, P <: Proposition, BX <: GenericBox[P, T]](val boxIdsToRemove: Set[Array[Byte]], val toAppend: Set[BX])


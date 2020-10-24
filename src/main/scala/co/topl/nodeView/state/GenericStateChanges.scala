package co.topl.nodeView.state

import co.topl.crypto.proposition.Proposition
import co.topl.nodeView.state.box.{BoxId, GenericBox}

class GenericStateChanges[T, P <: Proposition, BX <: GenericBox[P, T]](val boxIdsToRemove: Set[BoxId], val toAppend: Set[BX])


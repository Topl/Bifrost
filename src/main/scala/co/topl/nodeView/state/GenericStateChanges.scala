package co.topl.nodeView.state

import co.topl.nodeView.state.box.{BoxId, GenericBox}

class GenericStateChanges[BX <: GenericBox[_]](val boxIdsToRemove: Seq[BoxId], val toAppend: Seq[BX])


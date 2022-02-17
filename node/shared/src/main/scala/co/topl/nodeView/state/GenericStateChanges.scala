package co.topl.nodeView.state

import co.topl.modifier.box.{BoxId, GenericBox}

class GenericStateChanges[BX <: GenericBox[_]](val boxIdsToRemove: Seq[BoxId], val toAppend: Seq[BX])

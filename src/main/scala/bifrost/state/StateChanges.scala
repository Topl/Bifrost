package bifrost.state

import bifrost.modifier.box.proposition.Proposition
import bifrost.modifier.box.GenericBox

class StateChanges[T, P <: Proposition, BX <: GenericBox[P, T]](val boxIdsToRemove: Set[Array[Byte]], val toAppend: Set[BX])


package bifrost.state

import bifrost.modifier.box.proposition.Proposition
import bifrost.modifier.box.Box

class GenericStateChanges[T, P <: Proposition, BX <: Box[P, T]](val boxIdsToRemove: Set[Array[Byte]], val toAppend: Set[BX])


package bifrost.scorexMod

import bifrost.modifier.box.proposition.Proposition
import bifrost.modifier.box.GenericBox

class GenericStateChanges[T, P <: Proposition, BX <: GenericBox[P, T]] (val boxIdsToRemove: Set[Array[Byte]], val toAppend: Set[BX])


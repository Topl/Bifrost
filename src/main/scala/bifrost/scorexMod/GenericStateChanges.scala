package bifrost.scorexMod

import bifrost.transaction.box.proposition.Proposition

class GenericStateChanges[T, P <: Proposition, BX <: GenericBox[P, T]] (val boxIdsToRemove: Set[Array[Byte]], val toAppend: Set[BX])


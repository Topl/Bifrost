package bifrost.state

import bifrost.modifier.box.GenericBox
import bifrost.modifier.box.proposition.Proposition

class GenericStateChanges[T, P <: Proposition, BX <: GenericBox[P, T]](val boxIdsToRemove: Set[Array[Byte]], val toAppend: Set[BX])

trait TransactionAggregator

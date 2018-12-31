package bifrost.transaction.state

import bifrost.transaction.box.Box
import bifrost.transaction.box.proposition.Proposition

case class StateChanges[P <: Proposition, BX <: Box[P]] (boxIdsToRemove: Set[Array[Byte]], toAppend: Set[BX])

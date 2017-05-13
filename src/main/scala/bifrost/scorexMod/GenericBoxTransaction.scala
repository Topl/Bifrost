package bifrost.scorexMod

import com.google.common.primitives.Longs
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.BoxUnlocker
import scorex.core.transaction.box.proposition.Proposition

/**
  * Created by cykoz on 4/13/17.
  */
abstract class GenericBoxTransaction[P <: Proposition, T, BX <: GenericBox[P, T]] extends Transaction[P] {

  val unlockers: Traversable[BoxUnlocker[P]]
  val newBoxes: Traversable[BX]

  override lazy val messageToSign: Array[Byte] =
    (if(newBoxes.nonEmpty) newBoxes.map(_.bytes).reduce(_ ++ _) else Array[Byte]()) ++
      unlockers.map(_.closedBoxId).reduce(_ ++ _) ++
      Longs.toByteArray(timestamp) ++
      Longs.toByteArray(fee)
}

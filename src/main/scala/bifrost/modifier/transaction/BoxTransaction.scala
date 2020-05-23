package bifrost.modifier.transaction

import bifrost.modifier.box.proposition.Proposition
import bifrost.modifier.box.GenericBox
import bifrost.modifier.transaction.bifrostTransaction.GenericTransaction
import com.google.common.primitives.Longs

/**
  * Created by cykoz on 4/13/17.
  */
abstract class BoxTransaction[P <: Proposition, T, BX <: GenericBox[P, T]] extends GenericTransaction[P] {

  val newBoxes: Traversable[BX]

  override lazy val messageToSign: Array[Byte] =
    newBoxes.foldLeft(Array[Byte]())((acc, x) => acc ++ x.bytes)
      //unlockers.map(_.closedBoxId).reduce(_ ++ _) ++
    Longs.toByteArray(timestamp) ++
    Longs.toByteArray(fee)
}

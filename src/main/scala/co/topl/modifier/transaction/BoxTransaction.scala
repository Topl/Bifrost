package co.topl.modifier.transaction

import co.topl.nodeView.state.box.GenericBox
import co.topl.nodeView.state.box.proposition.Proposition
import com.google.common.primitives.Longs

/**
  * Created by cykoz on 4/13/17.
  */
abstract class BoxTransaction[P <: Proposition, T, BX <: GenericBox[P, T]] extends GenericTransaction[P] {

  self =>

  val newBoxes: Traversable[BX]

  override def messageToSign: Array[Byte] =
    self.newBoxes.foldLeft(Array[Byte]())((acc, x) => acc ++ x.bytes)
    Longs.toByteArray(self.timestamp) ++
    Longs.toByteArray(self.fee)
}

package co.topl.modifier.transaction

import co.topl.attestation.proposition.Proposition
import co.topl.nodeView.state.box.GenericBox
import com.google.common.primitives.Longs

/**
  * Created by cykoz on 4/13/17.
  */
abstract class BoxTransaction[T, P <: Proposition, BX <: GenericBox[T]] extends GenericTransaction[P] {

  self =>

  val newBoxes: Traversable[BX]

  override def messageToSign: Array[Byte] =
    self.newBoxes.foldLeft(Array[Byte]())((acc, x) => acc ++ x.bytes)
    Longs.toByteArray(self.timestamp) ++
    Longs.toByteArray(self.fee)
}

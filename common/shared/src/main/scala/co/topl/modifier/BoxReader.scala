package co.topl.modifier

import co.topl.modifier.box._

import scala.reflect.ClassTag

trait BoxReader[KP, KT] {

  def getBox(id: BoxId): Option[Box[_]]

  def getProgramBox[PBX <: ProgramBox: ClassTag](key: KP): Option[PBX]

  def getTokenBoxes(key: KT): Option[Seq[TokenBox[TokenValueHolder]]]

}

package co.topl.modifier

import co.topl.modifier.box.{Box, BoxId, ProgramBox, TokenBox, TokenValueHolder}

import scala.reflect.ClassTag

trait BoxReader[KP, KT] {

  def getBox(id: BoxId): Option[Box[_]]

  def getProgramBox[PBX <: ProgramBox: ClassTag](key: KP): Option[PBX]

  def getTokenBoxes(key: KT): Option[Seq[TokenBox[TokenValueHolder]]]

}

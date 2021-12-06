package co.topl.nodeView.state

import co.topl.modifier.BoxReader
import co.topl.modifier.box._
import co.topl.nodeView.NodeViewComponent
import co.topl.nodeView.state.MinimalState.VersionTag

import scala.reflect.ClassTag

trait StateReader[KP, KT] extends BoxReader[KP, KT] with NodeViewComponent {

  type ProgramKey = KP
  type TokenKey = KT

  val hasTBR: Boolean
  val hasPBR: Boolean
  val nodeKeys: Option[Set[KT]]

  // must be ID of last applied modifier
  def version: VersionTag

  def getBox(id: BoxId): Option[Box[_]]

  def getProgramBox[PBX <: ProgramBox: ClassTag](key: KP): Option[PBX]

  def getTokenBoxes(key: KT): Option[Seq[TokenBox[TokenValueHolder]]]

}

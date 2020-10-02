package bifrost.nodeView.state

import bifrost.nodeView.NodeViewComponent
import bifrost.nodeView.box.proposition.Proposition
import bifrost.nodeView.box.{ GenericBox, ProgramBox, TokenBox }
import bifrost.nodeView.state.MinimalState.VersionTag

import scala.reflect.ClassTag

trait StateReader[BX <: GenericBox[_ <: Proposition, _]] extends NodeViewComponent {

  type KP = ProgramBoxRegistry.K
  type KT = TokenBoxRegistry.K


  //must be ID of last applied modifier
  def version: VersionTag

  def getBox ( id: Array[Byte]): Option[BX]

  def getProgramBox[PBX <: ProgramBox : ClassTag] (key: KP): Option[PBX]

  def getTokenBoxes(key: KT): Option[Seq[TokenBox]]

  // JAA - commented out 2020.09.25 - I don't think we need this but it might be worth thinking about
  //def maxRollbackDepth: Int
}

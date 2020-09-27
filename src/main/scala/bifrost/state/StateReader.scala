package bifrost.state

import bifrost.modifier.box.{ GenericBox, ProgramBox, TokenBox }
import bifrost.modifier.box.proposition.Proposition
import bifrost.nodeView.NodeViewComponent
import bifrost.state.MinimalState.VersionTag

trait StateReader[BX <: GenericBox[_ <: Proposition, _]] extends NodeViewComponent {

  type KP = ProgramBoxRegistry.K
  type KT = TokenBoxRegistry.K


  //must be ID of last applied modifier
  def version: VersionTag

  def getBox ( id: Array[Byte]): Option[BX]

  def getProgramBox[PBX <: ProgramBox] (key: KP): Option[PBX]

  def getTokenBoxes(key: KT): Option[Seq[TokenBox]]

  // JAA - commented out 2020.09.25 - I don't think we need this but it might be worth thinking about
  //def maxRollbackDepth: Int
}

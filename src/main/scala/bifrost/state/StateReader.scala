package bifrost.state

import bifrost.modifier.box.GenericBox
import bifrost.modifier.box.proposition.Proposition
import bifrost.nodeView.NodeViewComponent
import bifrost.state.MinimalState.VersionTag

trait StateReader[BX <: GenericBox[P, T], P <: Proposition, T] extends NodeViewComponent {

  //must be ID of last applied modifier
  def version: VersionTag

  def getBox ( id: Array[Byte]): Option[BX]

  // JAA - commented out 2020.09.25 - I don't think we need this but it might be worth thinking about
  //def maxRollbackDepth: Int
}

package bifrost.state

import bifrost.nodeView.NodeViewComponent
import bifrost.state.MinimalState.VersionTag

trait StateReader extends NodeViewComponent {

  //must be ID of last applied modifier
  def version: VersionTag

  def maxRollbackDepth: Int
}

package co.topl.nodeView.nodeViewHolder

import co.topl.nodeView.NodeViewHolder
import co.topl.settings.{AppContext, AppSettings}
import co.topl.utils.NetworkType.NetworkPrefix

import scala.concurrent.ExecutionContext

class TestableNodeViewHolder(settings: AppSettings, appContext: AppContext)(implicit
  ec:                                  ExecutionContext,
  np:                                  NetworkPrefix
) extends NodeViewHolder(settings, appContext) {

  def nodeViewPublicAccessor: NodeView = (history(), minimalState(), memoryPool())

}

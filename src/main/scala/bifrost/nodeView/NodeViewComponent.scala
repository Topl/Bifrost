package bifrost.nodeView

trait NodeViewComponent {
  self =>

  type NVCT >: self.type <: NodeViewComponent

}

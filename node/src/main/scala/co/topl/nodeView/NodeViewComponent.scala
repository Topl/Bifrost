package co.topl.nodeView

trait NodeViewComponent {
  self =>

  type NVCT >: self.type <: NodeViewComponent

}

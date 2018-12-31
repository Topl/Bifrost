package bifrost

trait NodeViewComponent {
  self =>

  type NVCT >: self.type <: NodeViewComponent
}

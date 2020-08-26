package bifrost.nodeView

import bifrost.modifier.block.Block
import io.circe.Encoder

trait BifrostNodeViewModifier extends PersistentNodeViewModifier with NodeViewModifier

object BifrostNodeViewModifier {
  implicit val jsonEncoder: Encoder[BifrostNodeViewModifier] = {
    case b: Block ⇒ Block.jsonEncoder(b)
    case other => throw new Exception(s"Unknown persistent modifier type: $other")
  }
}

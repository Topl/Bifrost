package co.topl.nodeView

import co.topl.modifier.block.{Block, PersistentNodeViewModifier}
import io.circe.Encoder

trait BifrostNodeViewModifier extends PersistentNodeViewModifier

object BifrostNodeViewModifier {
  implicit val jsonEncoder: Encoder[BifrostNodeViewModifier] = {
    case b: Block ⇒ Block.jsonEncoder(b)
    case other => throw new Exception(s"Unknown persistent modifier type: $other")
  }
}

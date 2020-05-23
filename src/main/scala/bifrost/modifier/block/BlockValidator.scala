package bifrost.modifier.block

import scala.util.Try

trait BlockValidator[PM <: Block] {
  def validate(block: PM): Try[Unit]
}

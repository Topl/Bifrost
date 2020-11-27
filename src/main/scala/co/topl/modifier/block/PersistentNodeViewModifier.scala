package co.topl.modifier.block

import co.topl.modifier.block.PersistentNodeViewModifier.PNVMVersion
import co.topl.modifier.block.serialization.{BlockBodySerializer, BlockHeaderSerializer, BlockSerializer}
import co.topl.modifier.{ModifierId, NodeViewModifier}
import co.topl.utils.serialization.{BifrostSerializer, Reader, Writer}
import io.circe.Encoder

import scala.util.{Failure, Success}

trait PersistentNodeViewModifier extends NodeViewModifier {
  def parentId: ModifierId
  def version: PNVMVersion
}

object PersistentNodeViewModifier {
  type PNVMVersion = Byte


}

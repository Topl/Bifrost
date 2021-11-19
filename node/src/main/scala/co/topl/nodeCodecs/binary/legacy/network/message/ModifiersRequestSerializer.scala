package co.topl.nodeCodecs.binary.legacy.network.message

import co.topl.codecs.binary.legacy.modifier.ModifierIdSerializer
import co.topl.codecs.binary.legacy.{BifrostSerializer, Reader, Writer}
import co.topl.modifier.NodeViewModifier
import co.topl.modifier.NodeViewModifier.ModifierTypeId
import co.topl.network.message.Messages.MessagesV1
import co.topl.utils.Extensions.LongOps

class ModifiersRequestSerializer(maxInvObjects: Int) extends BifrostSerializer[MessagesV1.ModifiersRequest] {

  override def serialize(data: MessagesV1.ModifiersRequest, w: Writer): Unit = {
    val typeId = data.typeId
    val elems = data.ids
    require(elems.nonEmpty, "empty inv list")
    require(elems.lengthCompare(maxInvObjects) <= 0, s"more invs than $maxInvObjects in a message")
    w.put(typeId.value)
    w.putUInt(elems.size)
    elems.foreach { id =>
      val bytes = ModifierIdSerializer.toBytes(id)
      assert(bytes.length == NodeViewModifier.modifierIdSize)
      w.putBytes(bytes)
    }
  }

  override def parse(r: Reader): MessagesV1.ModifiersRequest = {
    val typeId = ModifierTypeId(r.getByte())
    val count = r.getUInt().toIntExact
    require(count > 0, "empty inv list")
    require(count <= maxInvObjects, s"$count elements in a message while limit is $maxInvObjects")
    val elems = (0 until count).map(_ => ModifierIdSerializer.parse(r))

    MessagesV1.ModifiersRequest(typeId, elems)
  }
}

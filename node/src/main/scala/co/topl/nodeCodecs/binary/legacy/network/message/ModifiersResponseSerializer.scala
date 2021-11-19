package co.topl.nodeCodecs.binary.legacy.network.message

import co.topl.codecs.binary.legacy.modifier.ModifierIdSerializer
import co.topl.codecs.binary.legacy.{BifrostSerializer, Reader, Writer}
import co.topl.modifier.NodeViewModifier
import co.topl.modifier.NodeViewModifier.ModifierTypeId
import co.topl.network.message.Messages.MessagesV1
import co.topl.utils.Extensions.LongOps
import co.topl.utils.Logging

class ModifiersResponseSerializer(maxMessageSize: Int)
    extends BifrostSerializer[MessagesV1.ModifiersResponse]
    with Logging {

  override def serialize(data: MessagesV1.ModifiersResponse, w: Writer): Unit = {

    val typeId = data.typeId
    val modifiers = data.modifiers
    require(modifiers.nonEmpty, "empty modifiers list")

    val (msgCount, msgSize) = modifiers.foldLeft((0, 5)) { case ((c, s), (_, modifier)) =>
      val size = s + NodeViewModifier.modifierIdSize + 4 + modifier.length
      val count = if (size <= maxMessageSize) c + 1 else c
      count -> size
    }

    val start = w.length()
    w.put(typeId.value)
    w.putUInt(msgCount)

    modifiers.take(msgCount).foreach { case (id, modifier) =>
      w.putBytes(ModifierIdSerializer.toBytes(id))
      w.putUInt(modifier.length)
      w.putBytes(modifier)
    }

    if (msgSize > maxMessageSize) {
      log.warn(
        s"Message with modifiers ${modifiers.keySet} have size $msgSize exceeding limit $maxMessageSize." +
        s" Sending ${w.length() - start} bytes instead"
      )
    }
  }

  override def parse(r: Reader): MessagesV1.ModifiersResponse = {
    val typeId = ModifierTypeId(r.getByte())
    val count = r.getUInt().toIntExact
    val seq = (0 until count).map { _ =>
      val id = ModifierIdSerializer.parse(r)
      val objBytesCnt = r.getUInt().toIntExact
      val obj = r.getBytes(objBytesCnt)
      id -> obj
    }
    MessagesV1.ModifiersResponse(typeId, seq.toMap)
  }
}

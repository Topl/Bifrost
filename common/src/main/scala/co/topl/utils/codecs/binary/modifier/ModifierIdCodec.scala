package co.topl.utils.codecs.binary.modifier

import co.topl.modifier.ModifierId
import co.topl.utils.codecs.binary.valuetypes.codecs.bytesCodec
import scodec.Codec

object ModifierIdCodec {
  val codec: Codec[ModifierId] = bytesCodec(ModifierId.size).as[ModifierId]
}

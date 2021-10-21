package co.topl.utils.codecs.binary.valuetypes

import co.topl.utils.codecs.binary.valuetypes.codecs._
import org.scalacheck.Gen

class UShortCodecSpec extends ValueTypesCodecCompatabilityBehavior {

  valueTypesCodecCompatabilityBehavior[UShort](
    "unsigned short",
    _ => uShortCodec,
    uShort => writer => writer.putUShort(uShort),
    _ => reader => reader.getUShort(),
    Gen.posNum[UShort]
  )
}

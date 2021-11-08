package co.topl.codecs.binary.scodecs.valuetypes

import co.topl.codecs.binary.scodecs.valuetypes.Types._
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

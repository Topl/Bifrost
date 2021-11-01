package co.topl.utils.codecs.binary.valuetypes

import co.topl.utils.codecs.binary.valuetypes.codecs._
import org.scalacheck.Gen

class ShortCodecSpec extends ValueTypesCodecCompatabilityBehavior {

  valueTypesCodecCompatabilityBehavior[Short](
    "short",
    _ => shortCodec,
    short => writer => writer.putShort(short),
    _ => reader => reader.getShort(),
    Gen.chooseNum[Short](Short.MinValue, Short.MaxValue)
  )
}

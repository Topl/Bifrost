package co.topl.codecs.binary.scodecs.valuetypes

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

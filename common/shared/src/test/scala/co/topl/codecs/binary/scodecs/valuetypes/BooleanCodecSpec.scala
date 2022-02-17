package co.topl.codecs.binary.scodecs.valuetypes

import org.scalacheck.Gen

class BooleanCodecSpec extends ValueTypesCodecCompatabilityBehavior {

  valueTypesCodecCompatabilityBehavior[Boolean](
    "boolean",
    _ => boolCodec,
    bool => writer => writer.putBoolean(bool),
    _ => reader => reader.getBoolean(),
    Gen.oneOf(true, false)
  )
}

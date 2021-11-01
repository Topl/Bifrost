package co.topl.utils.codecs.binary.valuetypes

import co.topl.utils.codecs.binary.valuetypes.codecs._
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

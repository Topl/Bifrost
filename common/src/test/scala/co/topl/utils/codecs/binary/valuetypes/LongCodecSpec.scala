package co.topl.utils.codecs.binary.valuetypes

import cats.implicits._
import co.topl.utils.codecs.binary.valuetypes.codecs._
import org.scalacheck.Gen

class LongCodecSpec extends ValueTypesCodecCompatabilityBehavior {

  valueTypesCodecCompatabilityBehavior[Long](
    "long",
    _ => longCodec,
    long => writer => writer.putLong(long),
    _ => reader => reader.getLong(),
    Gen.long
  )
}

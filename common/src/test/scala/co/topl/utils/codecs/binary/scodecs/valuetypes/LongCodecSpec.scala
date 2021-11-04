package co.topl.utils.codecs.binary.scodecs.valuetypes

import cats.implicits._
import co.topl.utils.codecs.binary.scodecs.valuetypes.Types._
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

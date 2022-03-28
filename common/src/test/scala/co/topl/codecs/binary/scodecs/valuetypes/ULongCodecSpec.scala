package co.topl.codecs.binary.scodecs.valuetypes

import cats.implicits._
import co.topl.codecs.binary.scodecs.valuetypes.Types.ULong
import org.scalacheck.Gen

class ULongCodecSpec extends ValueTypesCodecCompatabilityBehavior {

  valueTypesCodecCompatabilityBehavior[ULong](
    "unsigned long",
    _ => uLongCodec,
    uLong => writer => writer.putULong(uLong),
    _ => reader => reader.getULong(),
    Gen.chooseNum(0L, Long.MaxValue)
  )
}

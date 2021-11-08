package co.topl.codecs.binary.scodecs.valuetypes

import co.topl.codecs.binary.scodecs.valuetypes.Types._
import org.scalacheck.Gen

class IntCodecSpec extends ValueTypesCodecCompatabilityBehavior {

  valueTypesCodecCompatabilityBehavior[Int](
    "int",
    _ => intCodec,
    int => writer => writer.putInt(int),
    _ => reader => reader.getInt(),
    Gen.chooseNum[Int](Int.MinValue, Int.MaxValue)
  )
}

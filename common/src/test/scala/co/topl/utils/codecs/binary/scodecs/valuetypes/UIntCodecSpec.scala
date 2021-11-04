package co.topl.utils.codecs.binary.scodecs.valuetypes

import co.topl.utils.codecs.binary.scodecs.valuetypes.Types._
import org.scalacheck.Gen

class UIntCodecSpec extends ValueTypesCodecCompatabilityBehavior {

  valueTypesCodecCompatabilityBehavior[UInt](
    "unsigned int",
    _ => uIntCodec,
    uInt => writer => writer.putUInt(uInt),
    _ => reader => reader.getUInt(),
    Gen.posNum[UInt]
  )
}

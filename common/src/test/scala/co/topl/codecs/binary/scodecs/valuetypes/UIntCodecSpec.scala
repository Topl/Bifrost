package co.topl.codecs.binary.scodecs.valuetypes

import co.topl.codecs.binary.scodecs.valuetypes.Types.UInt
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

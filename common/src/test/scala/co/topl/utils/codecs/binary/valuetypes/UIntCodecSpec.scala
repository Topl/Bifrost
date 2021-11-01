package co.topl.utils.codecs.binary.valuetypes

import co.topl.utils.codecs.binary.valuetypes.codecs._
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

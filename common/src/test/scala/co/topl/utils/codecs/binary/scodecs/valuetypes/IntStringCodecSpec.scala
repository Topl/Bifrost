package co.topl.utils.codecs.binary.scodecs.valuetypes

import co.topl.utils.codecs.binary.scodecs.valuetypes.Types._
import org.scalacheck.Gen

class IntStringCodecSpec extends ValueTypesCodecCompatabilityBehavior {

  valueTypesCodecCompatabilityBehavior[IntString](
    "int string",
    _ => intStringCodec,
    intString => writer => writer.putIntString(intString),
    _ => reader => reader.getIntString(),
    Gen.asciiStr
  )
}

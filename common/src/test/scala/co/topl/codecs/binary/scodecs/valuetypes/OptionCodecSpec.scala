package co.topl.codecs.binary.scodecs.valuetypes

import co.topl.codecs.binary.scodecs.valuetypes.Types._
import org.scalacheck.Gen

class OptionCodecSpec extends ValueTypesCodecCompatabilityBehavior {

  valueTypesCodecCompatabilityBehavior[Option[ULong]](
    "option unsigned long",
    _ => optionCodec(uLongCodec),
    opt => writer => writer.putOption(opt)((w, uLong) => w.putULong(uLong)),
    _ => reader => reader.getOption(reader.getULong()),
    Gen.option(Gen.posNum[Long])
  )
}

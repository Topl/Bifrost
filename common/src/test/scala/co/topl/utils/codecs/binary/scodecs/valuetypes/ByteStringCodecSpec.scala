package co.topl.utils.codecs.binary.scodecs.valuetypes

import co.topl.utils.codecs.binary.scodecs.valuetypes.Types._
import co.topl.utils.encode.Base16
import org.scalacheck.Gen

class ByteStringCodecSpec extends ValueTypesCodecCompatabilityBehavior {

  valueTypesCodecCompatabilityBehavior[ByteString](
    "byte string",
    _ => byteStringCodec,
    str => writer => writer.putByteString(str),
    _ => reader => reader.getByteString(),
    Gen.posNum[Int].flatMap(int => Array(int.toByte)).filter(_.length < 256).map(Base16.encode)
  )
}

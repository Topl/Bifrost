package co.topl.utils.codecs.binary.valuetypes

import cats.implicits._
import co.topl.utils.CommonGenerators
import co.topl.utils.codecs.binary.valuetypes.codecs._
import org.scalacheck.Gen

class ByteCodecSpec extends ValueTypesCodecCompatabilityBehavior with CommonGenerators {

  valueTypesCodecCompatabilityBehavior[Byte](
    "byte",
    _ => byteCodec,
    byte => writer => writer.put(byte),
    _ => reader => reader.getByte(),
    Gen.posNum[Int].map(_.toByte)
  )

}

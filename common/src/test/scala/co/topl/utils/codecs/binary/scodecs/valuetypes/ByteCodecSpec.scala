package co.topl.utils.codecs.binary.scodecs.valuetypes

import cats.implicits._
import co.topl.utils.CommonGenerators
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

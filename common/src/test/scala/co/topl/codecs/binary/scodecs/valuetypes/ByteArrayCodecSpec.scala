package co.topl.codecs.binary.scodecs.valuetypes

import co.topl.utils.implicits._
import org.scalacheck.Gen

class ByteArrayCodecSpec extends ValueTypesCodecCompatabilityBehavior {

  valueTypesCodecCompatabilityBehavior[Array[Byte]](
    "bytes",
    bytes => bytesCodec(bytes.length),
    bytes => writer => writer.putBytes(bytes),
    bytes => reader => reader.getBytes(bytes.length),
    Gen.listOf(Gen.posNum[Int].map(_.toByte)).map(_.toArray)
  )
}

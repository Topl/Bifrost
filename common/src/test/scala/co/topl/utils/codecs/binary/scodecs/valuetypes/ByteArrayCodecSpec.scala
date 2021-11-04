package co.topl.utils.codecs.binary.scodecs.valuetypes

import cats.Eq
import org.scalacheck.Gen
import co.topl.utils.catsInstances._

class ByteArrayCodecSpec extends ValueTypesCodecCompatabilityBehavior {

  implicit private val eqBytes: Eq[Array[Byte]] = (a, b) => a sameElements b

  valueTypesCodecCompatabilityBehavior[Array[Byte]](
    "bytes",
    bytes => bytesCodec(bytes.length),
    bytes => writer => writer.putBytes(bytes),
    bytes => reader => reader.getBytes(bytes.length),
    Gen.listOf(Gen.posNum[Int].map(_.toByte)).map(_.toArray)
  )
}

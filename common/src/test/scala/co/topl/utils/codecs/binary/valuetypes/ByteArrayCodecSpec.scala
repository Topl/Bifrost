package co.topl.utils.codecs.binary.valuetypes

import cats.implicits._
import cats.{Eq, Show}
import co.topl.utils.codecs.binary.valuetypes.codecs._
import org.scalacheck.Gen
import co.topl.utils.codecs.binary.implicits._
import co.topl.utils.StringDataTypes.implicits._

class ByteArrayCodecSpec extends ValueTypesCodecCompatabilityBehavior {

  implicit private val showBytes: Show[Array[Byte]] = bytes => bytes.encodeAsBase16.show
  implicit private val eqBytes: Eq[Array[Byte]] = (a, b) => a sameElements b

  valueTypesCodecCompatabilityBehavior[Array[Byte]](
    "bytes",
    bytes => bytesCodec(bytes.length),
    bytes => writer => writer.putBytes(bytes),
    bytes => reader => reader.getBytes(bytes.length),
    Gen.listOf(Gen.posNum[Int].map(_.toByte)).map(_.toArray)
  )
}

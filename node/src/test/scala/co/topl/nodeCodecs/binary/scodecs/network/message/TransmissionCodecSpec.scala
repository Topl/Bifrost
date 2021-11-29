package co.topl.nodeCodecs.binary.scodecs.network.message

import co.topl.codecs.binary.CodecCompatabilityBehavior
import co.topl.network.message.{Transmission, TransmissionContent, TransmissionHeader}
import co.topl.nodeCatsInstances._
import co.topl.nodeCodecs.binary.legacy.network.message.TransmissionSerializer
import co.topl.nodeCodecs.binary.scodecs.Generators
import org.scalacheck.Gen
import scodec.bits.BitVector

class TransmissionCodecSpec extends CodecCompatabilityBehavior {
  val magicBytes: Array[Byte] = Array(1, 2, 3, 4)

  val transmissionGen: Gen[Transmission] =
    Generators.byteGen.flatMap(code =>
      Gen
        .option(
          Gen
            .listOfN(Transmission.headerLength, Generators.byteGen)
        )
        .map(bytes =>
          Transmission(
            TransmissionHeader(code, bytes.map(_.length).getOrElse(0)),
            bytes.map(someBytes => TransmissionContent(Transmission.checksum(someBytes.toArray), someBytes.toArray))
          )
        )
    )

  val failingTransmission = Transmission(TransmissionHeader(-10: Byte, 0), None)

  println(transmissionCodec(magicBytes).encode(failingTransmission).getOrElse(BitVector.empty))

  println(transmissionHeaderCodec(magicBytes).encode(failingTransmission.header).getOrElse(BitVector.empty))

  import co.topl.codecs._
  println(byteCodec.encode(-10: Byte))
  println(intCodec.encode(0))

  codecCompatabilityBehavior(
    "transmission",
    transmissionCodec(magicBytes),
    new TransmissionSerializer(magicBytes),
    Gen.const(failingTransmission)
  )
}

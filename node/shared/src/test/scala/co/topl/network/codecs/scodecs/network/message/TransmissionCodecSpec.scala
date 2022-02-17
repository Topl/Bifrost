package co.topl.network.codecs.scodecs.network.message

import co.topl.codecs.binary.CodecCompatabilityBehavior
import co.topl.network.codecs.scodecs.Generators
import co.topl.network.message.{Transmission, TransmissionContent, TransmissionHeader}
import co.topl.network.catsinstances.implicits._
import co.topl.network.codecs.legacy.message.TransmissionSerializer
import org.scalacheck.Gen

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

  codecCompatabilityBehavior(
    "transmission",
    transmissionCodec(magicBytes),
    new TransmissionSerializer(magicBytes),
    transmissionGen
  )
}

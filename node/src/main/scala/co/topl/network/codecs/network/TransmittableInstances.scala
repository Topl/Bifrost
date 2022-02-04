package co.topl.network.codecs.network

import co.topl.codecs.binary.typeclasses.Transmittable
import co.topl.network.message.Messages.MessagesV1
import co.topl.network.message.{Message, Transmission, TransmissionContent, TransmissionHeader}
import co.topl.network.codecs.scodecs._

trait TransmittableInstances {

  def transmissionHeaderTransmittable(magicBytes: Array[Byte]): Transmittable[TransmissionHeader] =
    Transmittable.fromCodec[TransmissionHeader](transmissionHeaderCodec(magicBytes))

  def transmissionContentTransmittable(dataLength: Int): Transmittable[TransmissionContent] =
    Transmittable.fromCodec[TransmissionContent](transmissionContentCodec(dataLength))

  def transmissionTransmittable(magicBytes: Array[Byte]): Transmittable[Transmission] =
    Transmittable.fromCodec[Transmission](transmissionCodec(magicBytes))

  implicit val messageTransmittable: Transmittable[Message] = Transmittable.fromCodec[Message]

  implicit val handshakeTransmittable: Transmittable[MessagesV1.Handshake] =
    Transmittable.fromCodec[MessagesV1.Handshake]
}

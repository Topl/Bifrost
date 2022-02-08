package co.topl.network.codecs.network

import co.topl.codecs.binary.typeclasses.Transmittable
import co.topl.network.message.Messages.MessagesV1
import co.topl.network.message.{Message, Transmission, TransmissionContent, TransmissionHeader}
import co.topl.network.codecs.scodecs._

trait TransmittableInstances {

  def transmissionHeaderTransmittable(magicBytes: Array[Byte]): Transmittable[TransmissionHeader] =
    Transmittable.instanceFromCodec[TransmissionHeader](transmissionHeaderCodec(magicBytes))

  def transmissionContentTransmittable(dataLength: Int): Transmittable[TransmissionContent] =
    Transmittable.instanceFromCodec[TransmissionContent](transmissionContentCodec(dataLength))

  def transmissionTransmittable(magicBytes: Array[Byte]): Transmittable[Transmission] =
    Transmittable.instanceFromCodec[Transmission](transmissionCodec(magicBytes))

  implicit val messageTransmittable: Transmittable[Message] = Transmittable.instanceFromCodec[Message]

  implicit val handshakeTransmittable: Transmittable[MessagesV1.Handshake] =
    Transmittable.instanceFromCodec[MessagesV1.Handshake]
}

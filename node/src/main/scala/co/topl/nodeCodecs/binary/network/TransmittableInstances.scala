package co.topl.nodeCodecs.binary.network

import co.topl.codecs.binary.typeclasses.Transmittable
import co.topl.network.message.Messages.MessagesV1
import co.topl.network.message.{Message, Transmission}
import co.topl.nodeCodecs.binary.scodecs._

trait TransmittableInstances {

  def transmissionTransmittable(magicBytes: Array[Byte]): Transmittable[Transmission] =
    Transmittable.fromCodec[Transmission](transmissionCodec(magicBytes))

  implicit val messageTransmittable: Transmittable[Message] = Transmittable.fromCodec[Message]

  implicit val handshakeTransmittable: Transmittable[MessagesV1.Handshake] =
    Transmittable.fromCodec[MessagesV1.Handshake]
}

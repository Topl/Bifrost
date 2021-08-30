package co.topl.stakeholder.remote

import co.topl.network.message.Message.MessageCode
import co.topl.network.message.MessageSpecV1
import co.topl.utils.serialization.{Reader, Writer}
import co.topl.stakeholder.components.SerializationMethods
import co.topl.stakeholder.components.Serializer.DeserializeMac
import co.topl.stakeholder.primitives.{ByteStream, Mac}
import co.topl.stakeholder.remote.SpecTypes.diffuseCode

/**
  * AMS 2020:
  * Diffuse data is used to identify peers with their forging public address, optional to broadcast
  */

object DiffuseDataSpec extends MessageSpecV1[(Mac,Array[Byte])] with SerializationMethods {
  override val messageCode: MessageCode = diffuseCode
  override val messageName: String = "Diffuse"

  override def parse(r: Reader):(Mac,Array[Byte]) = {
    val mac = {
      fromBytes(new ByteStream(r.getBytes(mac_length),DeserializeMac)) match {
        case result:Mac@unchecked => result
      }
    }
    (mac,r.getBytes(r.remaining))
  }

  override def serialize(obj: (Mac,Array[Byte]), w: Writer): Unit = {
    w.putBytes(getBytes(obj._1))
    w.putBytes(obj._2)
  }
}

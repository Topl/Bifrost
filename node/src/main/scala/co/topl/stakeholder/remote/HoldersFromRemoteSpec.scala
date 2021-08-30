package co.topl.stakeholder.remote

import co.topl.network.message.Message.MessageCode
import co.topl.network.message.MessageSpecV1
import co.topl.utils.serialization.{Reader, Writer}
import co.topl.stakeholder.components.SerializationMethods
import co.topl.stakeholder.components.Serializer.DeserializeHoldersFromRemote
import co.topl.stakeholder.primitives.ByteStream
import co.topl.stakeholder.remote.SpecTypes.{holdersFromRemote,HoldersType}

/**
  * AMS 2020:
  * Peers broadcast local stakeholders, first step of peer discovery
  * Critical for peer discovery at the actor level, lets holders become aware of all active participants
  */

object HoldersFromRemoteSpec extends MessageSpecV1[HoldersType] with SerializationMethods {
  override val messageCode: MessageCode = holdersFromRemote
  override val messageName: String = "Holders from remote"

  override def parse(r: Reader): HoldersType = {
    holdersFromBytes(r.getBytes(r.remaining))
  }

  override def serialize(obj: HoldersType, w: Writer): Unit = {
    w.putBytes(holdersToBytes(obj))
  }

  def holdersFromBytes(bytes: Array[Byte]): HoldersType = {
    val msgBytes = new ByteStream(bytes,DeserializeHoldersFromRemote)
    fromBytes(msgBytes) match {
      case msg:HoldersType@unchecked => msg
    }
  }

  def holdersToBytes(msg: HoldersType): Array[Byte] = {
    getHoldersBytes(msg)
  }
}
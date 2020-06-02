package bifrost.old_network.message

import bifrost.utils.serialization.Serializer

trait MessageSpec[Content] extends Serializer[Content] {
  val messageCode: Message.MessageCode
  val messageName: String

  override def toString: String = s"MessageSpec($messageCode: $messageName)"
}

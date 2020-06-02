package bifrost.old_network.message

import bifrost.utils.serialization.BifrostSerializer

trait MessageSpec[Content] extends BifrostSerializer[Content] {
  val messageCode: Message.MessageCode
  val messageName: String

  override def toString: String = s"MessageSpec($messageCode: $messageName)"
}

package bifrost.settings

import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}

object ApplicationVersionSerializer extends BifrostSerializer[Version] {
  val SerializedVersionLength: Int = 3

  override def serialize(obj: Version, w: Writer): Unit = {
    w.put(obj.firstDigit)
    w.put(obj.secondDigit)
    w.put(obj.thirdDigit)
  }

  override def parse(r: Reader): Version = {
    Version(
      r.getByte(),
      r.getByte(),
      r.getByte()
    )
  }
}

package co.topl.settings

import co.topl.utils.serialization.{BytesSerializable, _}

/**
  * Version of p2p protocol. Node can only process messages of it's version or lower.
  */
class Version( val firstDigit: Byte,
               val secondDigit: Byte,
               val thirdDigit: Byte
             ) extends BytesSerializable with Ordered[Version] {

  override type M = Version

  val blockByte: Byte = firstDigit

  override def serializer: BifrostSerializer[Version] = VersionSerializer

  override def compare(that: Version): Int =
    if (this.firstDigit != that.firstDigit) {
      this.firstDigit - that.firstDigit
    } else if (this.secondDigit != that.secondDigit) {
      this.secondDigit - that.secondDigit
    } else {
      this.thirdDigit - that.thirdDigit
    }
}

object Version {
  def apply(v: String): Version = {
    val split = v.split("\\.")
    new Version(split(0).toByte, split(1).toByte, split(2).toByte)
  }

  val initial: Version = new Version(0, 0, 1)
  val last: Version = new Version(0, 0, 1)
}

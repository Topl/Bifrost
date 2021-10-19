package co.topl.settings

import co.topl.utils.codecs.binary.legacy.{BifrostSerializer, BytesSerializable}

/**
 * Version of blockchain protocol
 *
 * @param firstDigit Significant hard fork/version change/consensus rule set change
 * @param secondDigit Feature additions, bugs fixing hard forks, minor hard forks, significant soft forks,
 *                    interface should work the same
 * @param thirdDigit Minor changes
 */
class Version(val firstDigit: Byte, val secondDigit: Byte, val thirdDigit: Byte)
    extends BytesSerializable
    with Ordered[Version] {

  override type M = Version

  lazy val blockByte: Byte = firstDigit

  override def serializer: BifrostSerializer[Version] = VersionSerializer

  override def toString: String = s"${firstDigit.toString}.${secondDigit.toString}.${thirdDigit.toString}"

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

  val initial: Version = new Version(0, 0, 1)
  val MaxValue: Version = new Version(Byte.MaxValue, Byte.MaxValue, Byte.MaxValue)

  def apply(value: String): Version = {
    val split = value.split("\\.")
    new Version(split(0).toByte, split(1).toByte, split(2).toByte)
  }
}

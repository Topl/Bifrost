package co.topl.settings

import co.topl.codecs.binary.legacy.{BifrostSerializer, Reader, Writer}

/** Serializer for Version class */
object VersionSerializer extends BifrostSerializer[Version] {

  override def serialize(obj: Version, w: Writer): Unit = {
    w.put(obj.firstDigit)
    w.put(obj.secondDigit)
    w.put(obj.thirdDigit)
  }

  override def parse(r: Reader): Version =
    new Version(
      r.getByte(),
      r.getByte(),
      r.getByte()
    )
}

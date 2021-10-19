package co.topl.utils.codecs.binary.legacy.modifier.box

import co.topl.modifier.box.SecurityRoot
import co.topl.utils.codecs.binary.legacy.{BifrostSerializer, Reader, Writer}

object SecurityRootSerializer extends BifrostSerializer[SecurityRoot] {

  override def serialize(obj: SecurityRoot, w: Writer): Unit =
    w.putBytes(obj.root)

  override def parse(r: Reader): SecurityRoot = {
    val root: Array[Byte] = r.getBytes(SecurityRoot.size)
    new SecurityRoot(root)
  }
}

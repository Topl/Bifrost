package co.topl.modifier.box

import co.topl.codecs.binary.legacy.modifier.box.SecurityRootSerializer
import co.topl.codecs.binary.legacy.{BifrostSerializer, BytesSerializable}
import co.topl.crypto.hash.digest.Digest32
import co.topl.utils.encode.Base58
import com.google.common.primitives.Ints

case class SecurityRoot(root: Array[Byte]) extends BytesSerializable {

  require(root.length == SecurityRoot.size, "Invalid securityRoot length")

  @deprecated
  type M = SecurityRoot

  @deprecated
  override def serializer: BifrostSerializer[SecurityRoot] = SecurityRootSerializer

  def getRoot: Array[Byte] = root

  override def hashCode(): Int = Ints.fromByteArray(root)

  override def equals(obj: Any): Boolean = obj match {
    case sr: SecurityRoot => sr.root sameElements root
    case _                => false
  }

  override def toString: String = Base58.encode(root)
}

object SecurityRoot {

  val size: Int = Digest32.size // 32 bytes
  val empty: SecurityRoot = new SecurityRoot(Array.fill(size)(0: Byte))
}

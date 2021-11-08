package co.topl.modifier.box

import cats.implicits._
import co.topl.crypto.hash.digest.Digest32
import co.topl.utils.StringDataTypes.Base58Data
import co.topl.utils.catsInstances._
import co.topl.codecs._
import co.topl.codecs.binary.legacy.modifier.box.SecurityRootSerializer
import co.topl.codecs.binary.legacy.{BifrostSerializer, BytesSerializable}
import com.google.common.primitives.Ints
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder}

case class SecurityRoot(root: Array[Byte]) extends BytesSerializable {

  require(root.length == SecurityRoot.size, "Invalid securityRoot")

  @deprecated
  type M = SecurityRoot

  @deprecated
  override def serializer: BifrostSerializer[SecurityRoot] = SecurityRootSerializer

  def getRoot: Array[Byte] = root

  @deprecated
  override def hashCode(): Int = Ints.fromByteArray(root)

  @deprecated
  override def equals(obj: Any): Boolean = obj match {
    case sr: SecurityRoot => sr.root sameElements root
    case _                => false
  }

  @deprecated
  override def toString: String = root.show
}

object SecurityRoot {

  val size: Int = Digest32.size // 32 bytes
  val empty: SecurityRoot = new SecurityRoot(Array.fill(size)(0: Byte))
}
